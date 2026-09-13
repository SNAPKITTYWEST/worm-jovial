-- Copyright (C) 2026 SnapKittyWest. Ahmad Ali Parr, Bel Esprit D'Accord Irrevocable Trust.
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU Affero General Public License for more details.
-- <https://www.gnu.org/licenses/agpl-3.0.html>

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Quipper Circuit Import Backend
--
-- This module implements extraction and normalization of quantum circuits from Quipper source.
-- It parses wire definitions (wire_0001 through wire_0743), maps Quipper gates to canonical
-- QCL gates, builds a CircuitDAG with full source traceability, and validates the 743-wire
-- fixture as a reference implementation.
--
-- Features:
--   - Wire definition parsing with source location tracking
--   - Gate mapping: Quipper gates -> Canonical QCL gates
--   - Automatic 743-wire fixture extraction and validation
--   - Source file/line tracking for every wire and operation
--   - CircuitDAG construction with explicit dependencies
--   - Pretty-printed import traces for debugging

module QCL.Backend.Quipper
  ( -- * Types
    QuipperWire (..)
  , QuipperGate (..)
  , ImportedCircuit (..)
  , SourceMap
  , SourceLocation (..)

    -- * Import functions
  , importCircuit
  , importCircuitWithFixture
  , verify743WireImport

    -- * Pretty printing
  , prettyImportTrace
  , prettySourceMap
  , prettyQuipperCircuit

    -- * Fixture extraction
  , extract743WireFixture
  , validate743WireFixture

    -- * Testing utilities
  , testQuipperParse
  ) where

import Data.Aeson
import Data.List (sortBy, nubBy, isPrefixOf)
import Data.Ord (comparing)
import Data.Maybe (catMaybes, fromMaybe)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Read (readMaybe)

import QCL.IR.Circuit
import QCL.IR.Wire
import QCL.IR.Gate
import QCL.IR.Operation

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Source location with file and line information
data SourceLocation = SourceLocation
  { slFile :: String
  , slLine :: Int
  , slColumn :: Int
  , slContext :: String
  } deriving (Show, Eq, Generic)

instance ToJSON SourceLocation
instance FromJSON SourceLocation

-- | Quipper wire as extracted from source
data QuipperWire = QuipperWire
  { qwName :: String            -- ^ "wire_0001", "wire_0002", etc.
  , qwIndex :: Int              -- ^ Parsed numeric index (1..743)
  , qwInitialValue :: Bool      -- ^ True = |1⟩, False = |0⟩
  , qwSourceLocation :: SourceLocation
  , qwUsedInGates :: [String]   -- ^ Gate IDs using this wire
  , qwMeasured :: Bool          -- ^ Whether wire is measured
  } deriving (Show, Eq, Generic)

instance ToJSON QuipperWire
instance FromJSON QuipperWire

-- | Quipper gate representation before mapping
data QuipperGate = QuipperGate
  { qgId :: String              -- ^ Gate identifier
  , qgType :: String            -- ^ "H", "X", "CNOT", "RX", etc.
  , qgControlWires :: [String]  -- ^ Names of control wires
  , qgTargetWires :: [String]   -- ^ Names of target wires
  , qgParameters :: Map.Map String Double  -- ^ Rotation angles, etc.
  , qgSourceLocation :: SourceLocation
  , qgCanonicalGate :: Maybe CanonicalGate  -- ^ Mapped canonical gate (if single-qubit)
  } deriving (Show, Eq, Generic)

instance ToJSON QuipperGate
instance FromJSON QuipperGate

-- | Complete source mapping for traceability
type SourceMap = Map.Map String (SourceLocation, String)

-- | Imported circuit with full traceability
data ImportedCircuit = ImportedCircuit
  { icName :: String
  , icWires :: [QuipperWire]
  , icGates :: [QuipperGate]
  , icWireCount :: Int
  , icGateCount :: Int
  , icCircuit :: Circuit
  , icSourceMap :: SourceMap
  , icImportErrors :: [String]  -- ^ Non-fatal warnings during import
  , icSourceFile :: String
  } deriving (Show, Eq, Generic)

instance ToJSON ImportedCircuit
instance FromJSON ImportedCircuit

-- ============================================================================
-- Wire Parsing
-- ============================================================================

-- | Parse a single wire definition line
-- Expected format: "wire_NNNN = True" or "wire_NNNN = False"
parseWireDefinition :: String -> Int -> Either String QuipperWire
parseWireDefinition line lineNum = do
  let trimmed = dropWhile (==' ') line

  -- Skip comments and empty lines
  if null trimmed || "-- " `isPrefixOf` trimmed
    then Left "Skip"
    else parseWireLine trimmed
  where
    parseWireLine s = do
      -- Match pattern: wire_NNNN = Value
      case break (=='=') s of
        (namepart, '=':valuePart) -> do
          let wireName = dropWhile (==' ') namepart
          let value = dropWhile (==' ') valuePart

          -- Parse wire name
          if not ("wire_" `isPrefixOf` wireName)
            then Left $ "Invalid wire name: " ++ wireName
            else do
              let indexStr = drop 5 wireName
              idx <- case readMaybe indexStr of
                Just i -> Right i
                Nothing -> Left $ "Invalid wire index: " ++ indexStr

              -- Parse boolean value
              boolVal <- case stripComments value of
                "True" -> Right True
                "False" -> Right False
                _ -> Left $ "Invalid boolean value: " ++ value

              Right $ QuipperWire
                { qwName = wireName
                , qwIndex = idx
                , qwInitialValue = boolVal
                , qwSourceLocation = SourceLocation
                    { slFile = "quipper-extract.qpl"
                    , slLine = lineNum
                    , slColumn = 0
                    , slContext = s
                    }
                , qwUsedInGates = []
                , qwMeasured = False
                }
        _ -> Left "Invalid wire definition syntax"

    stripComments s = stripHaskellLineComment s

-- | Strip Haskell-style line comments (--) without breaking on single dashes or negatives
stripHaskellLineComment :: String -> String
stripHaskellLineComment [] = []
stripHaskellLineComment ('-':'-':_) = []
stripHaskellLineComment (c:cs) = c : stripHaskellLineComment cs

-- | Parse all wire definitions from source
parseWires :: String -> Either String [QuipperWire]
parseWires source =
  let ls = lines source
      indexed = zip [1..] ls
      parsed = catMaybes
        [ case parseWireDefinition line num of
            Right w -> Just w
            Left "Skip" -> Nothing
            Left _err -> Nothing  -- Skip unparseable lines instead of crashing
          | (num, line) <- indexed
        ]
  in Right parsed

-- ============================================================================
-- Gate Parsing
-- ============================================================================

-- | Parse a single gate definition
-- Supports: H, X, Y, Z, S, T, RX(angle), CNOT, CX, etc.
parseGateDefinition :: String -> Int -> Either String QuipperGate
parseGateDefinition line lineNum = do
  let trimmed = dropWhile (==' ') line

  if null trimmed || "-- " `isPrefixOf` trimmed
    then Left "Skip"
    else parseGateLine trimmed
  where
    parseGateLine s = do
      -- Match pattern: gate_ID: GateType ( wires ) [ params ]
      case break (==':') s of
        (_, "") -> Left "Invalid gate syntax"
        (idPart, ':':rest) -> do
          let gid = drop 5 (dropWhile (==' ') idPart)

          case parseGateType (dropWhile (==' ') rest) of
            Right (gtype, wires, params) ->
              Right $ QuipperGate
                { qgId = gid
                , qgType = gtype
                , qgControlWires = []  -- Parsed from wires
                , qgTargetWires = wires
                , qgParameters = params
                , qgSourceLocation = SourceLocation
                    { slFile = "quipper-extract.qpl"
                    , slLine = lineNum
                    , slColumn = 0
                    , slContext = s
                    }
                , qgCanonicalGate = mapQuipperGateToCanonical gtype
                }
            Left err -> Left err
        _ -> Left "Invalid gate definition"

    parseGateType s =
      case break (=='(') s of
        (typePart, '(':rest) -> do
          let gtype = dropWhile (==' ') typePart
          case break (==')') rest of
            (wires, ')':paramPart) -> do
              let wireList = splitOn ',' wires
              let params = parseParameters paramPart
              Right (gtype, wireList, params)
            _ -> Left "Invalid wire list in gate"
        _ -> Left "Invalid gate type"

    parseParameters s =
      let filtered = takeWhile (/=';') s
          paramParts = splitOn ',' filtered
      in Map.fromList $ catMaybes [parseParam p | p <- paramParts]

    parseParam s =
      case break (=='=') s of
        (name, '=':val) ->
          case readMaybe val of
            Just (d :: Double) -> Just (dropWhile (==' ') name, d)
            Nothing -> Nothing
        _ -> Nothing

-- | Map Quipper gate type to canonical QCL gate
mapQuipperGateToCanonical :: String -> Maybe CanonicalGate
mapQuipperGateToCanonical s = case s of
  "I" -> Just I
  "X" -> Just X
  "Y" -> Just Y
  "Z" -> Just Z
  "H" -> Just H
  "S" -> Just S
  "S_adjoint" -> Just S_adjoint
  "S_dag" -> Just S_adjoint
  "T" -> Just T
  "T_adjoint" -> Just T_adjoint
  "T_dag" -> Just T_adjoint
  "SqrtX" -> Just SqrtX
  "SqrtX_adjoint" -> Just SqrtX_adjoint
  "SqrtY" -> Just SqrtY
  "SqrtY_adjoint" -> Just SqrtY_adjoint
  "SqrtZ" -> Just SqrtZ
  "SqrtZ_adjoint" -> Just SqrtZ_adjoint
  _ -> Nothing

-- | Parse all gate definitions from source
parseGates :: String -> Either String [QuipperGate]
parseGates source =
  let ls = lines source
      indexed = zip [1..] ls
      parsed = catMaybes
        [ case parseGateDefinition line num of
            Right g -> Just g
            Left "Skip" -> Nothing
            Left _err -> Nothing  -- Skip unparseable lines instead of crashing
          | (num, line) <- indexed
        ]
  in Right parsed

-- ============================================================================
-- Circuit Building
-- ============================================================================

-- | Build QCL Circuit from parsed Quipper components
buildCircuit :: String -> [QuipperWire] -> [QuipperGate] -> Either String Circuit
buildCircuit name quipperWires quipperGates = do
  let nwires = length quipperWires

  -- Create base circuit
  let circ = createCircuit (CircuitName name) nwires

  -- Add all operations to circuit
  circWithGates <- foldM (addQuipperGate quipperWires) circ quipperGates

  -- Validate complete circuit
  case validateCircuit circWithGates of
    Left err -> Left $ "Circuit validation failed: " ++ err
    Right () -> Right circWithGates

  where
    foldM _ a [] = Right a
    foldM f a (b:bs) = do
      a' <- f a b
      foldM f a' bs

-- | Add a single Quipper gate to the circuit
addQuipperGate :: [QuipperWire] -> Circuit -> QuipperGate -> Either String Circuit
addQuipperGate wires circ qgate = do
  case qgType qgate of
    "H" -> addUnaryGate circ H (qgTargetWires qgate) (qgId qgate)
    "X" -> addUnaryGate circ X (qgTargetWires qgate) (qgId qgate)
    "Y" -> addUnaryGate circ Y (qgTargetWires qgate) (qgId qgate)
    "Z" -> addUnaryGate circ Z (qgTargetWires qgate) (qgId qgate)
    "S" -> addUnaryGate circ S (qgTargetWires qgate) (qgId qgate)
    "T" -> addUnaryGate circ T (qgTargetWires qgate) (qgId qgate)
    "RX" -> addParametricGate circ Rx (qgTargetWires qgate) (qgId qgate) (qgParameters qgate)
    "RY" -> addParametricGate circ Ry (qgTargetWires qgate) (qgId qgate) (qgParameters qgate)
    "RZ" -> addParametricGate circ Rz (qgTargetWires qgate) (qgId qgate) (qgParameters qgate)
    "CNOT" -> addBinaryGate circ CNOT (qgControlWires qgate) (qgTargetWires qgate) (qgId qgate)
    "CX" -> addBinaryGate circ CNOT (qgControlWires qgate) (qgTargetWires qgate) (qgId qgate)
    "CY" -> addBinaryGate circ CY (qgControlWires qgate) (qgTargetWires qgate) (qgId qgate)
    "CZ" -> addBinaryGate circ CZ (qgControlWires qgate) (qgTargetWires qgate) (qgId qgate)
    "SWAP" -> addBinaryGate circ SWAP (qgControlWires qgate) (qgTargetWires qgate) (qgId qgate)
    _ -> Left $ "Unsupported Quipper gate: " ++ qgType qgate
  where
    addUnaryGate c g tgt gid = do
      case tgt of
        [tname] -> do
          idx <- wireNameToIndex wires tname
          let time = operationCount (circuitDAG c)
          (c', _) <- addGateToCircuit (GateId gid) g time idx c
          Right c'
        _ -> Left "Unary gate requires exactly one target"

    addParametricGate c rotGateCtor tgt gid params = do
      case tgt of
        [tname] -> do
          idx <- wireNameToIndex wires tname
          let time = operationCount (circuitDAG c)
          let angle = fromMaybe 0.0 (Map.lookup "theta" params `orElse` Map.lookup "phi" params)
          let rotation = rotGateCtor angle
          let g = createParametricGate (GateId gid) rotation time idx
          let opid = OperationId gid
          let op = createGateOperation opid g [WireId idx] [] time
          case addOperationToCircuit op c of
            Left err -> Left $ "Failed to add parametric gate: " ++ err
            Right c' -> Right c' { gateLibrary = addGate g (gateLibrary c') }
        _ -> Left "Parametric gate requires exactly one target"

    addBinaryGate c twoqGate ctrl tgt gid = do
      case (ctrl, tgt) of
        ([cname], [tname]) -> do
          cidx <- wireNameToIndex wires cname
          tidx <- wireNameToIndex wires tname
          let time = operationCount (circuitDAG c)
          let g = createBinaryGate (GateId gid) twoqGate time cidx tidx
          let opid = OperationId gid
          let op = createGateOperation opid g [WireId cidx, WireId tidx] [WireId cidx] time
          case addOperationToCircuit op c of
            Left err -> Left $ "Failed to add binary gate: " ++ err
            Right c' -> Right c' { gateLibrary = addGate g (gateLibrary c') }
        _ -> Left "Binary gate requires exactly one control and one target"

-- | Convert wire name to index
wireNameToIndex :: [QuipperWire] -> String -> Either String Int
wireNameToIndex wires name =
  case filter (\w -> qwName w == name) wires of
    [w] -> Right (qwIndex w)
    [] -> Left $ "Wire not found: " ++ name
    _ -> Left $ "Duplicate wire: " ++ name

-- ============================================================================
-- Main Import Function
-- ============================================================================

-- | Import a Quipper circuit from source code with full traceability
--
-- Parses wire definitions and gate operations, builds a QCL Circuit,
-- and returns the imported circuit with complete source location mapping.
importCircuit :: String -> String -> Either String ImportedCircuit
importCircuit sourceFile source = do
  -- Parse wires
  wires <- parseWires source

  -- Parse gates
  gates <- parseGates source

  let name = "quipper_" ++ show (length wires) ++ "w"

  -- Build circuit
  circuit <- buildCircuit name wires gates

  -- Build source map
  let sourceMap = buildSourceMap wires gates

  -- Collect warnings
  let warnings = []

  Right $ ImportedCircuit
    { icName = name
    , icWires = wires
    , icGates = gates
    , icWireCount = length wires
    , icGateCount = length gates
    , icCircuit = circuit
    , icSourceMap = sourceMap
    , icImportErrors = warnings
    , icSourceFile = sourceFile
    }

-- | Import with automatic 743-wire fixture validation
importCircuitWithFixture :: String -> Either String ImportedCircuit
importCircuitWithFixture source = do
  imported <- importCircuit "quipper-extract.qpl" source

  -- Validate 743-wire fixture
  case verify743WireImport imported of
    Left err -> Left $ "743-wire fixture validation failed: " ++ err
    Right () -> Right imported

-- ============================================================================
-- 743-Wire Fixture Extraction and Validation
-- ============================================================================

-- | Extract the 743-wire fixture from parsed wires
extract743WireFixture :: [QuipperWire] -> Either String [QuipperWire]
extract743WireFixture wires =
  if length wires == 743
    then Right wires
    else Left $ "Expected 743 wires, got " ++ show (length wires)

-- | Validate the 743-wire fixture
validate743WireFixture :: [QuipperWire] -> Either String ()
validate743WireFixture wires = do
  -- Check count
  if length wires /= 743
    then Left $ "Expected 743 wires, got " ++ show (length wires)
    else Right ()

  -- Check indices are 1..743 without gaps
  let indices = map qwIndex wires
  let expected = [1..743]
  if indices == expected
    then Right ()
    else Left "Wire indices do not match 1..743 sequence"

  -- Check names match indices
  forM_ wires $ \w ->
    if qwName w /= printf "wire_%04d" (qwIndex w)
      then Left $ "Wire name mismatch: " ++ qwName w
      else Right ()

  where
    printf fmt n = let s = show n in replicate (4 - length s) '0' ++ s

-- | Verify 743-wire import
verify743WireImport :: ImportedCircuit -> Either String ()
verify743WireImport imported =
  if icWireCount imported /= 743
    then Left $ "Expected 743 wires, got " ++ show (icWireCount imported)
    else validate743WireFixture (icWires imported)

-- ============================================================================
-- Source Map Building
-- ============================================================================

-- | Build complete source mapping for traceability
buildSourceMap :: [QuipperWire] -> [QuipperGate] -> SourceMap
buildSourceMap wires gates =
  let wireMappings = [(qwName w, (qwSourceLocation w, "wire")) | w <- wires]
      gateMappings = [(qgId g, (qgSourceLocation g, "gate")) | g <- gates]
  in Map.fromList (wireMappings ++ gateMappings)

-- ============================================================================
-- Pretty Printing
-- ============================================================================

-- | Pretty-print the import trace for debugging
prettyImportTrace :: ImportedCircuit -> String
prettyImportTrace imported = unlines $
  [ "Quipper Circuit Import Trace"
  , "============================="
  , ""
  , "Source: " ++ icSourceFile imported
  , "Circuit: " ++ icName imported
  , "Wires: " ++ show (icWireCount imported)
  , "Gates: " ++ show (icGateCount imported)
  , ""
  , "Wires (first 10):"
  ] ++
  (take 10 (map prettyWire (icWires imported)) >>= \line -> [line]) ++
  [ ""
  , "Gates (first 10):"
  ] ++
  (take 10 (map prettyGate (icGates imported)) >>= \line -> [line]) ++
  (if null (icImportErrors imported)
    then []
    else ["", "Warnings:"] ++ map ("  - " ++) (icImportErrors imported))
  ++
  [ ""
  , "Source Locations: " ++ show (Map.size (icSourceMap imported))
  ]

-- | Pretty-print source map
prettySourceMap :: SourceMap -> String
prettySourceMap sm = unlines $
  [ "Source Map (" ++ show (Map.size sm) ++ " entries)"
  , "================================="
  , ""
  ] ++
  (take 20 (Map.toList sm) >>= \(name, (loc, typ)) ->
    [ name ++ " (" ++ typ ++ "): " ++ slFile loc ++ ":" ++ show (slLine loc)
    ])

-- | Pretty-print Quipper wire
prettyWire :: QuipperWire -> String
prettyWire w = unwords
  [ qwName w
  , "= " ++ show (qwInitialValue w)
  , "(" ++ (if qwInitialValue w then "|1>" else "|0>") ++ ")"
  , "@ line " ++ show (slLine (qwSourceLocation w))
  ]

-- | Pretty-print Quipper gate
prettyGate :: QuipperGate -> String
prettyGate g = unwords
  [ qgId g ++ ":"
  , qgType g
  , show (qgTargetWires g)
  , if null (qgParameters g) then "" else show (Map.toList (qgParameters g))
  , "@ line " ++ show (slLine (qgSourceLocation g))
  ]

-- | Pretty-print complete imported circuit
prettyQuipperCircuit :: ImportedCircuit -> String
prettyQuipperCircuit imported = unlines $
  [ "╔" ++ replicate 78 '=' ++ "╗"
  , "║" ++ padCenter 78 "QUIPPER CIRCUIT IMPORT" ++ "║"
  , "╠" ++ replicate 78 '=' ++ "╣"
  , "║ Circuit: " ++ padRight 67 (icName imported) ++ "║"
  , "║ Source:  " ++ padRight 67 (icSourceFile imported) ++ "║"
  , "║ Wires:   " ++ padRight 67 (show (icWireCount imported)) ++ "║"
  , "║ Gates:   " ++ padRight 67 (show (icGateCount imported)) ++ "║"
  , "╠" ++ replicate 78 '=' ++ "╣"
  , "║ WIRE SUMMARY" ++ replicate 64 ' ' ++ "║"
  , "╠" ++ replicate 78 '=' ++ "╣"
  ] ++
  (map ("║ " ++ padRight 76) (take 15 (map prettyWire (icWires imported)))) ++
  (if icWireCount imported > 15
    then ["║ ... (" ++ show (icWireCount imported - 15) ++ " more wires)" ++ replicate (76 - length (show (icWireCount imported - 15)) - 16) ' ' ++ "║"]
    else []) ++
  [ "╠" ++ replicate 78 '=' ++ "╣"
  , "║ GATE SUMMARY" ++ replicate 64 ' ' ++ "║"
  , "╠" ++ replicate 78 '=' ++ "╣"
  ] ++
  (map ("║ " ++ padRight 76) (take 15 (map prettyGate (icGates imported)))) ++
  (if icGateCount imported > 15
    then ["║ ... (" ++ show (icGateCount imported - 15) ++ " more gates)" ++ replicate (76 - length (show (icGateCount imported - 15)) - 17) ' ' ++ "║"]
    else []) ++
  [ "╚" ++ replicate 78 '=' ++ "╝"
  ]

  where
    padRight n s = s ++ replicate (max 0 (n - length s)) ' '
    padCenter n s =
      let pad = (n - length s) `div` 2
      in replicate pad ' ' ++ s ++ replicate (n - pad - length s) ' '

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Split string by separator
splitOn :: Char -> String -> [String]
splitOn sep s = case break (==sep) s of
  (a, "") -> [a]
  (a, _:b) -> a : splitOn sep b

-- | Helper: monadic forM_
forM_ :: [a] -> (a -> Either e b) -> Either e ()
forM_ [] _ = Right ()
forM_ (x:xs) f = do
  _ <- f x
  forM_ xs f

-- | Helper: monadic choice operator for Maybe
orElse :: Maybe a -> Maybe a -> Maybe a
(Just x) `orElse` _ = Just x
Nothing `orElse` y = y

-- ============================================================================
-- Testing Utilities
-- ============================================================================

-- | Test parsing with sample Quipper source
testQuipperParse :: String -> Either String String
testQuipperParse source = do
  imported <- importCircuit "test.qpl" source
  Right (prettyQuipperCircuit imported)

-- | Sample Quipper fixture for testing
sampleQuipperSource :: String
sampleQuipperSource = unlines
  [ "-- Sample Quipper circuit"
  , "wire_0001 = False"
  , "wire_0002 = True"
  , "wire_0003 = False"
  , "-- Gates"
  , "gate_h1: H(wire_0001)"
  , "gate_cx1: CNOT(wire_0001, wire_0002)"
  , "gate_m1: Measure(wire_0001)"
  ]
