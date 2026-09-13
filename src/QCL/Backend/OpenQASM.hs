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

-- | OpenQASM 2.0 Backend
--
-- Export quantum circuits to OpenQASM 2.0 standard format.
-- Supports:
--   - Register declarations (qreg, creg)
--   - Canonical gate operations
--   - Parameterized rotation gates
--   - Two and three-qubit gates
--   - Measurements and resets
--   - Barriers and synchronization
--
-- OpenQASM 2.0 specification: https://github.com/Qiskit/openqasm

module QCL.Backend.OpenQASM
  ( OpenQASMProgram(..)
  , exportCircuit
  , registerDeclarations
  , operationsToQASM
  , validateOpenQASM
  , gateToQASM
  , operationToQASM
  , formatQASMGate
  , formatUnaryGate
  , formatParametricGate
  , formatBinaryGate
  , formatTernaryGate
  , countQubitsInQASM
  , exportCircuitWithMetadata
  ) where

import Data.Aeson
import Data.List (sortBy, nub)
import Data.Ord (comparing)
import GHC.Generics
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set

import QCL.IR.Circuit
import QCL.IR.Operation
import QCL.IR.Wire
import QCL.IR.Gate
import QCL.IR.Register

-- | OpenQASM program representation
data OpenQASMProgram = OpenQASMProgram
  { qasmVersion :: String              -- ^ "OPENQASM 2.0"
  , qasmIncludes :: [String]           -- ^ Include statements
  , qasmRegisters :: [String]          -- ^ Register declarations
  , qasmOperations :: [String]         -- ^ Gate operations and measurements
  , qasmComments :: [String]           -- ^ Metadata comments
  } deriving (Show, Eq, Generic)

instance ToJSON OpenQASMProgram
instance FromJSON OpenQASMProgram

-- | Export circuit to OpenQASM 2.0 program
exportCircuit :: Circuit -> String
exportCircuit circ = unlines $ concat
  [ qasmHeader
  , registerDeclarations circ
  , [""]
  , operationsToQASM circ
  ]
  where
    qasmHeader =
      [ "OPENQASM 2.0;"
      , "include \"qelib1.inc\";"
      ]

-- | Generate register declarations for a circuit
registerDeclarations :: Circuit -> [String]
registerDeclarations circ =
  let stats = case getCircuitStats circ of
        Right s -> s
        Left _ -> CircuitStats 0 0 0 0 0 0 0

      -- Extract register information from WireRegister
      qubits = statsQubitCount stats
      measurements = statsMeasurementCount stats

      -- Main quantum register
      qregDecl = if qubits > 0
                 then ["qreg q[" ++ show qubits ++ "];"]
                 else []

      -- Classical register for measurement results (same size as measurements or qubits)
      cregSize = if measurements > 0 then measurements else qubits
      cregDecl = if cregSize > 0
                 then ["creg c[" ++ show cregSize ++ "];"]
                 else []
  in qregDecl ++ cregDecl

-- | Convert all circuit operations to OpenQASM instructions
operationsToQASM :: Circuit -> [String]
operationsToQASM circ =
  let ops = getAllOperations (circuitDAG circ)
  in concatMap (operationToQASM circ) ops

-- | Convert a single operation to OpenQASM instruction
operationToQASM :: Circuit -> Operation -> [String]
operationToQASM circ op =
  case opType op of
    GateOperation -> case gate op of
      Just g -> gateToQASM g (targetWires op ++ controlWires op)
      Nothing -> []

    MeasurementOperation ->
      let (WireId idx) = case targetWires op of
            [] -> WireId 0
            (w:_) -> w
          measureInstr = printf "measure q[%d] -> c[%d];" idx idx
      in [measureInstr]

    ResetOperation ->
      let instrs = map (\(WireId idx) -> printf "reset q[%d];" idx) (targetWires op)
      in instrs

    BarrierOperation ->
      if null (targetWires op)
      then ["barrier q;"]
      else
        let wires = map (\(WireId i) -> printf "q[%d]" i) (targetWires op)
            wireList = unwords wires
        in ["barrier " ++ wireList ++ ";"]

-- | Convert a gate to OpenQASM instruction(s)
gateToQASM :: Gate -> [WireId] -> [String]
gateToQASM g wires =
  let gType = gateType g
  in case gType of
    UnaryGate cg -> formatUnaryGate cg wires
    ParametricGate rg -> formatParametricGate rg wires
    BinaryGate tg -> formatBinaryGate tg wires
    TernaryGate tg -> formatTernaryGate tg wires
    MeasurementGate _ -> []  -- Handled separately in operationToQASM
    ResetGate _ -> []         -- Handled separately in operationToQASM

-- | Format unary gate to OpenQASM
formatUnaryGate :: CanonicalGate -> [WireId] -> [String]
formatUnaryGate gate wires =
  case wires of
    [] -> []
    (WireId idx : _) ->
      let gateName = case gate of
            I -> "id"
            X -> "x"
            Y -> "y"
            Z -> "z"
            H -> "h"
            S -> "s"
            S_adjoint -> "sdg"
            T -> "t"
            T_adjoint -> "tdg"
            SqrtX -> "sx"
            SqrtX_adjoint -> "sxdg"
            SqrtY -> "sy"
            SqrtY_adjoint -> "sydg"
            SqrtZ -> "sz"
            SqrtZ_adjoint -> "szdg"
      in [printf "%s q[%d];" gateName idx]

-- | Format parametric (rotation) gate to OpenQASM
formatParametricGate :: RotationGate -> [WireId] -> [String]
formatParametricGate gate wires =
  case wires of
    [] -> []
    (WireId idx : _) ->
      case gate of
        Rx angle -> [printf "rx(%.6g) q[%d];" angle idx]
        Ry angle -> [printf "ry(%.6g) q[%d];" angle idx]
        Rz angle -> [printf "rz(%.6g) q[%d];" angle idx]
        PhaseShift angle -> [printf "p(%.6g) q[%d];" angle idx]

-- | Format binary (two-qubit) gate to OpenQASM
formatBinaryGate :: TwoQubitGate -> [WireId] -> [String]
formatBinaryGate gate wires =
  case wires of
    [] -> []
    [_] -> []  -- Need two wires
    (WireId c : WireId t : _) ->
      case gate of
        CNOT -> [printf "cx q[%d],q[%d];" c t]
        CY -> [printf "cy q[%d],q[%d];" c t]
        CZ -> [printf "cz q[%d],q[%d];" c t]
        SWAP -> [printf "swap q[%d],q[%d];" c t]
        iSWAP -> [printf "iswap q[%d],q[%d];" c t]
        XX angle -> [printf "rxx(%.6g) q[%d],q[%d];" angle c t]
        YY angle -> [printf "ryy(%.6g) q[%d],q[%d];" angle c t]
        ZZ angle -> [printf "rzz(%.6g) q[%d],q[%d];" angle c t]

-- | Format ternary (three-qubit) gate to OpenQASM
formatTernaryGate :: ThreeQubitGate -> [WireId] -> [String]
formatTernaryGate gate wires =
  case wires of
    [] -> []
    [_] -> []
    [_, _] -> []  -- Need three wires
    (WireId a : WireId b : WireId c : _) ->
      case gate of
        CCX -> [printf "ccx q[%d],q[%d],q[%d];" a b c]  -- Toffoli
        CSwap -> [printf "cswap q[%d],q[%d],q[%d];" a b c]
    _ -> []

-- | Helper: format a complete gate instruction with parameters
formatQASMGate :: String -> [Int] -> Maybe Double -> String
formatQASMGate gateName wireIndices paramMaybe =
  case (wireIndices, paramMaybe) of
    ([], _) -> ""
    ([w], Nothing) -> printf "%s q[%d];" gateName w
    ([w], Just p) -> printf "%s(%.6g) q[%d];" gateName p w
    ([c, t], Nothing) -> printf "%s q[%d],q[%d];" gateName c t
    ([c, t], Just p) -> printf "%s(%.6g) q[%d],q[%d];" gateName p c t
    ([a, b, c], Nothing) -> printf "%s q[%d],q[%d],q[%d];" gateName a b c
    _ -> ""

-- | Validate OpenQASM 2.0 format string
validateOpenQASM :: String -> Either String ()
validateOpenQASM content = do
  let lines' = lines content

  -- Check header
  case lines' of
    [] -> Left "Empty OpenQASM program"
    (header : rest) ->
      if not (header == "OPENQASM 2.0;")
      then Left $ "Invalid OpenQASM header: " ++ header
      else validateRegisters rest

-- | Validate register declarations
validateRegisters :: [String] -> Either String ()
validateRegisters [] = Right ()
validateRegisters (line : rest)
  | "include" `elem` words line = validateRegisters rest
  | "qreg" `elem` words line =
      if validRegisterDecl line
      then validateRegisters rest
      else Left $ "Invalid qreg declaration: " ++ line
  | "creg" `elem` words line =
      if validRegisterDecl line
      then validateRegisters rest
      else Left $ "Invalid creg declaration: " ++ line
  | otherwise = Right ()  -- Rest are operations

-- | Check if a register declaration is valid
validRegisterDecl :: String -> Bool
validRegisterDecl line =
  let trimmed = takeWhile (/= ';') line
      tokens = words trimmed
  in case tokens of
    [_, name] -> validRegisterName name
    _ -> False

-- | Check if register name has valid syntax: name[size]
validRegisterName :: String -> Bool
validRegisterName s =
  case break (== '[') s of
    (name, '[' : rest) ->
      not (null name) &&
      case break (== ']') rest of
        (sizeStr, "]") -> all (\c -> c `elem` "0123456789") sizeStr && not (null sizeStr)
        _ -> False
    _ -> False

-- | Count qubits in OpenQASM program
countQubitsInQASM :: String -> Either String Int
countQubitsInQASM content =
  let qregLines = filter (("qreg" `elem`) . words) (lines content)
  in case qregLines of
    [] -> Left "No qreg declaration found"
    (line : _) -> extractRegisterSize line

-- | Extract size from register declaration
extractRegisterSize :: String -> Either String Int
extractRegisterSize line =
  let withoutSemicolon = takeWhile (/= ';') line
      startBracket = dropWhile (/= '[') withoutSemicolon
  in case startBracket of
    ('[' : rest) ->
      case break (== ']') rest of
        (sizeStr, "]") ->
          case reads sizeStr of
            [(n, "")] -> Right n
            _ -> Left $ "Invalid register size: " ++ sizeStr
        _ -> Left "Malformed register declaration"
    _ -> Left "No bracket found in register declaration"

-- | Export circuit with full metadata
exportCircuitWithMetadata :: Circuit -> OpenQASMProgram
exportCircuitWithMetadata circ =
  let stats = case getCircuitStats circ of
        Right s -> s
        Left _ -> CircuitStats 0 0 0 0 0 0 0

      (CircuitName name) = circuitName circ

      comments =
        [ "// Circuit: " ++ name
        , "// Qubits: " ++ show (statsQubitCount stats)
        , "// Depth: " ++ show (statsDepth stats)
        , "// Gates: " ++ show (statsUnitaryCount stats)
        , "// Measurements: " ++ show (statsMeasurementCount stats)
        ]
  in OpenQASMProgram
    { qasmVersion = "OPENQASM 2.0"
    , qasmIncludes = ["qelib1.inc"]
    , qasmRegisters = registerDeclarations circ
    , qasmOperations = operationsToQASM circ
    , qasmComments = comments
    }
