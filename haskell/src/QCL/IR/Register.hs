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

-- | Quantum Register model
--
-- Named collections of qubits organized for:
--   - Data qubits (problem qubits)
--   - Ancilla qubits (auxiliary/helper)
--   - Classical bits (measurement results)
--   - Register composition (nested registers)
--
-- Provides high-level abstraction above raw wire IDs.

module QCL.IR.Register where

import Data.Aeson
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set

import QCL.IR.Wire

-- | Register name
newtype RegisterName = RegisterName String
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RegisterName
instance FromJSON RegisterName

-- | Register type
data RegisterType
  = QRegType             -- ^ Qubit register (quantum data)
  | AncillaRegister      -- ^ Ancilla register (helper qubits)
  | ClassicalRegister    -- ^ Classical bit register (measurement results)
  | CompositeRegister    -- ^ Nested composite register
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RegisterType
instance FromJSON RegisterType

-- | Register element (one qubit or bit)
data RegisterElement
  = QuantumElement WireId
  | ClassicalElement Int
  deriving (Show, Eq, Generic)

instance ToJSON RegisterElement
instance FromJSON RegisterElement

-- | Quantum register (named collection of qubits)
data QuantumRegister = QuantumRegister
  { regName :: RegisterName
  , regType :: RegisterType
  , regSize :: Int                      -- ^ Number of qubits
  , regWires :: [WireId]                -- ^ Ordered wire IDs
  , regBase :: Maybe Int                -- ^ Base index (for register slicing)
  , regParent :: Maybe RegisterName     -- ^ Parent register (if composite)
  , description :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON QuantumRegister
instance FromJSON QuantumRegister

-- | Classical register for measurement results
data ClassicalRegister = ClassicalRegister
  { crName :: RegisterName
  , crSize :: Int                       -- ^ Number of classical bits
  , crBits :: [Int]                     -- ^ Classical bit values (0 or 1)
  , crMeasuredFrom :: Maybe RegisterName  -- ^ Which quantum register was measured
  } deriving (Show, Eq, Generic)

instance ToJSON ClassicalRegister
instance FromJSON ClassicalRegister

-- | Register mapping (for convenient access)
data RegisterMapping = RegisterMapping
  { quantumRegs :: Map.Map RegisterName QuantumRegister
  , classicalRegs :: Map.Map RegisterName ClassicalRegister
  , nameToWires :: Map.Map String [WireId]  -- ^ Quick lookup: register name → wires
  , wireToName :: Map.Map WireId String     -- ^ Quick lookup: wire → register name
  } deriving (Show, Eq, Generic)

instance ToJSON RegisterMapping
instance FromJSON RegisterMapping

-- | Create empty register mapping
emptyRegisterMapping :: RegisterMapping
emptyRegisterMapping = RegisterMapping
  { quantumRegs = Map.empty
  , classicalRegs = Map.empty
  , nameToWires = Map.empty
  , wireToName = Map.empty
  }

-- | Create a quantum register
createQuantumRegister :: RegisterName -> [WireId] -> Maybe String -> QuantumRegister
createQuantumRegister name wires desc = QuantumRegister
  { regName = name
  , regType = QRegType
  , regSize = length wires
  , regWires = wires
  , regBase = Nothing
  , regParent = Nothing
  , description = desc
  }

-- | Create an ancilla register
createAncillaRegister :: RegisterName -> [WireId] -> Maybe String -> QuantumRegister
createAncillaRegister name wires desc = QuantumRegister
  { regName = name
  , regType = AncillaRegister
  , regSize = length wires
  , regWires = wires
  , regBase = Nothing
  , regParent = Nothing
  , description = desc
  }

-- | Create a classical register
createClassicalRegister :: RegisterName -> Int -> ClassicalRegister
createClassicalRegister name size = ClassicalRegister
  { crName = name
  , crSize = size
  , crBits = replicate size 0
  , crMeasuredFrom = Nothing
  }

-- | Create a register slice (subset of wires)
sliceRegister :: RegisterName -> QuantumRegister -> Int -> Int -> Either String QuantumRegister
sliceRegister newName reg start end = do
  if start < 0 || end > regSize reg || start > end
    then Left $ "Invalid slice: start=" ++ show start ++ ", end=" ++ show end ++ ", size=" ++ show (regSize reg)
    else Right $ QuantumRegister
      { regName = newName
      , regType = regType reg
      , regSize = end - start
      , regWires = take (end - start) (drop start (regWires reg))
      , regBase = Just start
      , regParent = Just (regName reg)
      , description = Just $ "Slice of " ++ show (regName reg) ++ "[" ++ show start ++ ":" ++ show end ++ "]"
      }

-- | Register indexing: get wire at index
indexRegister :: QuantumRegister -> Int -> Either String WireId
indexRegister reg idx = do
  if idx < 0 || idx >= regSize reg
    then Left $ "Index out of bounds: " ++ show idx ++ " in register of size " ++ show (regSize reg)
    else Right (regWires reg !! idx)

-- | Get multiple wires from register
indexRegisterMultiple :: QuantumRegister -> [Int] -> Either String [WireId]
indexRegisterMultiple reg indices = mapM (indexRegister reg) indices

-- | Add quantum register to mapping
addQuantumRegister :: QuantumRegister -> RegisterMapping -> Either String RegisterMapping
addQuantumRegister qreg regmap =
  let (RegisterName name) = regName qreg
      nameStr = name
  in if Map.member (regName qreg) (quantumRegs regmap)
     then Left $ "Register already exists: " ++ nameStr
     else Right regmap
       { quantumRegs = Map.insert (regName qreg) qreg (quantumRegs regmap)
       , nameToWires = Map.insert nameStr (regWires qreg) (nameToWires regmap)
       , wireToName = foldl (\m w -> Map.insert w nameStr m) (wireToName regmap) (regWires qreg)
       }

-- | Add classical register
addClassicalRegister :: RegisterName -> Int -> RegisterMapping -> Either String RegisterMapping
addClassicalRegister name size regmap = do
  if Map.member name (classicalRegs regmap)
    then Left "Classical register already exists"
    else Right regmap
      { classicalRegs = Map.insert name (ClassicalRegister name size (replicate size 0) Nothing) (classicalRegs regmap)
      }

-- | Get quantum register by name
getQuantumRegister :: RegisterName -> RegisterMapping -> Maybe QuantumRegister
getQuantumRegister name regmap = Map.lookup name (quantumRegs regmap)

-- | Get classical register by name
getClassicalRegister :: RegisterName -> RegisterMapping -> Maybe ClassicalRegister
getClassicalRegister name regmap = Map.lookup name (classicalRegs regmap)

-- | Get wires for register by name
getRegisterWires :: String -> RegisterMapping -> Maybe [WireId]
getRegisterWires name regmap = Map.lookup name (nameToWires regmap)

-- | Get register name for wire
getWireRegister :: WireId -> RegisterMapping -> Maybe String
getWireRegister wire regmap = Map.lookup wire (wireToName regmap)

-- | Get all quantum registers
getAllQuantumRegisters :: RegisterMapping -> [QuantumRegister]
getAllQuantumRegisters regmap = Map.elems (quantumRegs regmap)

-- | Get all classical registers
getAllClassicalRegisters :: RegisterMapping -> [ClassicalRegister]
getAllClassicalRegisters regmap = Map.elems (classicalRegs regmap)

-- | Standard register names (common convention)
data StandardRegisterNames = StandardRegisterNames
  { qReg :: RegisterName        -- ^ Main quantum register ("q")
  , aReg :: RegisterName        -- ^ Ancilla register ("a")
  , cReg :: RegisterName        -- ^ Classical register ("c")
  } deriving (Show, Eq, Generic)

instance ToJSON StandardRegisterNames
instance FromJSON StandardRegisterNames

-- | Create standard registers
createStandardRegisters :: Int -> Int -> RegisterMapping
createStandardRegisters nQubits nAncilla =
  let qubits = [WireId i | i <- [1..nQubits]]
      ancillas = [WireId i | i <- [nQubits + 1..nQubits + nAncilla]]
      qReg = createQuantumRegister (RegisterName "q") qubits (Just "Main quantum register")
      aReg = createAncillaRegister (RegisterName "a") ancillas (Just "Ancilla qubits")
  in case addQuantumRegister qReg emptyRegisterMapping of
    Left _ -> emptyRegisterMapping
    Right regmap1 -> case addQuantumRegister aReg regmap1 of
      Left _ -> regmap1
      Right regmap2 -> case addClassicalRegister (RegisterName "c") nQubits regmap2 of
        Left _ -> regmap2
        Right regmap3 -> regmap3

-- | Composite register (for hierarchical organization)
data CompositeRegister = CompositeRegister
  { compName :: RegisterName
  , compRegisters :: [RegisterName]  -- ^ Child registers
  , compSize :: Int                  -- ^ Total size
  } deriving (Show, Eq, Generic)

instance ToJSON CompositeRegister
instance FromJSON CompositeRegister

-- | Register statistics
data RegisterStats = RegisterStats
  { statsQuantumTotal :: Int
  , statsAncillaTotal :: Int
  , statsClassicalTotal :: Int
  , statsRegisterCount :: Int
  , statsAverageSize :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON RegisterStats
instance FromJSON RegisterStats

-- | Compute statistics
getRegisterStats :: RegisterMapping -> RegisterStats
getRegisterStats regmap =
  let qRegs = getAllQuantumRegisters regmap
      cRegs = getAllClassicalRegisters regmap
      qTotal = sum [regSize r | r <- qRegs, regType r == QRegType]
      aTotal = sum [regSize r | r <- qRegs, regType r == AncillaRegister]
      cTotal = sum [crSize r | r <- cRegs]
      totalRegs = length qRegs + length cRegs
      avgSize = if totalRegs == 0 then 0 else fromIntegral (qTotal + aTotal + cTotal) / fromIntegral totalRegs
  in RegisterStats qTotal aTotal cTotal totalRegs avgSize

-- | Validate register mapping
validateRegisterMapping :: RegisterMapping -> Either String ()
validateRegisterMapping regmap = do
  -- Check no duplicate wires
  let allWires = Set.fromList $ concat [regWires r | r <- getAllQuantumRegisters regmap]
  let wireCount = length $ concat [regWires r | r <- getAllQuantumRegisters regmap]
  if Set.size allWires /= wireCount
    then Left "Duplicate wires in registers"
    else Right ()

  -- Check no register name collisions
  let qNames = Set.fromList [regName r | r <- getAllQuantumRegisters regmap]
  let cNames = Set.fromList [crName r | r <- getAllClassicalRegisters regmap]
  if Set.size (Set.intersection qNames cNames) > 0
    then Left "Register name collision between quantum and classical"
    else Right ()

  Right ()

-- | Register rename
renameRegister :: RegisterName -> RegisterName -> RegisterMapping -> Either String RegisterMapping
renameRegister oldName newName regmap = do
  case getQuantumRegister oldName regmap of
    Just qreg -> do
      let oldQRegs = quantumRegs regmap
      let newQRegs = Map.delete oldName $ Map.insert newName (qreg { regName = newName }) oldQRegs
      let (RegisterName oldStr) = oldName
      let (RegisterName newStr) = newName
      let newNameToWires = Map.delete oldStr $ Map.insert newStr (fromMaybe [] (getRegisterWires oldStr regmap)) (nameToWires regmap)
      Right regmap
        { quantumRegs = newQRegs
        , nameToWires = newNameToWires
        }
    Nothing -> case getClassicalRegister oldName regmap of
      Just creg -> do
        let oldCRegs = classicalRegs regmap
        let newCRegs = Map.delete oldName $ Map.insert newName (creg { crName = newName }) oldCRegs
        Right regmap { classicalRegs = newCRegs }
      Nothing -> Left $ "Register not found: " ++ show oldName

-- | Convert register to string representation
registerToString :: QuantumRegister -> String
registerToString reg =
  let (RegisterName name) = regName reg
      typeStr = case regType reg of
        QRegType -> "quantum"
        AncillaRegister -> "ancilla"
        ClassicalRegister -> "classical"
        CompositeRegister -> "composite"
  in name ++ "[" ++ show (regSize reg) ++ "] : " ++ typeStr

-- | Register slicing with Haskell-style syntax
-- Example: q[0:5] returns first 5 qubits
parseRegisterSlice :: String -> Either String (RegisterName, Maybe (Int, Int))
parseRegisterSlice s =
  case break (== '[') s of
    (name, "") -> Right (RegisterName name, Nothing)
    (name, '[':rest) ->
      case break (== ']') rest of
        (slice, "]") ->
          case break (== ':') slice of
            (start, "") -> case reads start of
              [(n, "")] -> Right (RegisterName name, Just (n, n + 1))
              _ -> Left $ "Invalid slice syntax: " ++ s
            (start, ':':end) -> case (reads start, reads end) of
              ([(s', "")], [(e', "")]) -> Right (RegisterName name, Just (s', e'))
              _ -> Left $ "Invalid slice syntax: " ++ s
            _ -> Left $ "Invalid slice syntax: " ++ s
        _ -> Left $ "Invalid slice syntax: " ++ s

-- | 743-wire standard register (for quantum-crypto-lang fixture)
create743WireRegisters :: RegisterMapping
create743WireRegisters =
  let wires = [WireId i | i <- [1..743]]
      -- Split into main (700) and ancilla (43)
      mainWires = take 700 wires
      ancillaWires = drop 700 wires
      mainReg = createQuantumRegister (RegisterName "main") mainWires (Just "Main 700-qubit register")
      ancillaReg = createAncillaRegister (RegisterName "aux") ancillaWires (Just "43 ancilla qubits")
  in case addQuantumRegister mainReg emptyRegisterMapping of
    Left _ -> emptyRegisterMapping
    Right regmap -> case addQuantumRegister ancillaReg regmap of
      Left _ -> regmap
      Right regmap' -> case addClassicalRegister (RegisterName "out") 700 regmap' of
        Left _ -> regmap'
        Right regmap'' -> regmap''
