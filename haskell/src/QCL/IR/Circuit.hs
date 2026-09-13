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

-- | Quantum Circuit representation
--
-- Complete circuit composition: Wires + Gates + Operations + DAG
-- Forms the canonical intermediate representation (IR) for compilation.

module QCL.IR.Circuit where

import Data.Aeson
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set

import QCL.IR.Wire
import QCL.IR.Gate
import QCL.IR.Operation

-- | Circuit name and metadata
newtype CircuitName = CircuitName String
  deriving (Show, Eq, Generic)

instance ToJSON CircuitName
instance FromJSON CircuitName

-- | Circuit parameter binding
data CircuitParameter = CircuitParameter
  { paramName :: String
  , paramValue :: Double
  , paramMin :: Maybe Double
  , paramMax :: Maybe Double
  , paramUnit :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON CircuitParameter
instance FromJSON CircuitParameter

-- | Complete quantum circuit
data Circuit = Circuit
  { circuitName :: CircuitName
  , wireRegister :: WireRegister
  , gateLibrary :: GateLibrary
  , circuitDAG :: CircuitDAG
  , parameters :: [CircuitParameter]
  , createdAt :: String
  , version :: String
  , source :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON Circuit
instance FromJSON Circuit

-- | Create empty circuit
createEmptyCircuit :: CircuitName -> Circuit
createEmptyCircuit name = Circuit
  { circuitName = name
  , wireRegister = emptyWireRegister
  , gateLibrary = emptyGateLibrary
  , circuitDAG = emptyCircuitDAG
  , parameters = []
  , createdAt = "2026-09-12"
  , version = "1.0"
  , source = Nothing
  }

-- | Create circuit with specified register size
createCircuit :: CircuitName -> Int -> Circuit
createCircuit name nqubits = Circuit
  { circuitName = name
  , wireRegister = createWireRegister nqubits
  , gateLibrary = createStandardGateLibrary
  , circuitDAG = emptyCircuitDAG
  , parameters = []
  , createdAt = "2026-09-12"
  , version = "1.0"
  , source = Nothing
  }

-- | Add operation to circuit
addOperationToCircuit :: Operation -> Circuit -> Either String Circuit
addOperationToCircuit op circ = do
  -- Validate wires exist
  let wires = wireIds op
  forM_ wires $ \wire -> do
    case getWire wire (wireRegister circ) of
      Nothing -> Left $ "Wire not found: " ++ show wire
      Just _ -> Right ()

  -- Add operation to DAG
  let newDAG = addOperation op (circuitDAG circ)

  -- Update wire states (transition to Live)
  newWireReg <- foldM (\wr w -> transitionWireState w Live 0 wr) (wireRegister circ) wires

  Right $ circ
    { wireRegister = newWireReg
    , circuitDAG = newDAG
    }

-- | Add gate to circuit
addGateToCircuit :: GateId -> CanonicalGate -> Int -> Int -> Circuit -> Either String (Circuit, Operation)
addGateToCircuit gid gate time targetWire circ = do
  -- Get wire
  case getWire (WireId targetWire) (wireRegister circ) of
    Nothing -> Left $ "Target wire not found: " ++ show targetWire
    Just _ -> do
      -- Create gate in library
      let g = createUnaryGate gid gate time targetWire
      let newLib = addGate g (gateLibrary circ)

      -- Create operation
      let opid = OperationId (show gid)
      let op = createGateOperation opid g [WireId targetWire] [] time

      -- Add to circuit
      newCirc <- addOperationToCircuit op circ

      Right (newCirc { gateLibrary = newLib }, op)

-- | Add measurement to circuit
addMeasurementToCircuit :: OperationId -> MeasurementBasis -> Int -> Int -> Circuit -> Either String (Circuit, Operation)
addMeasurementToCircuit opid basis time wire circ = do
  case getWire (WireId wire) (wireRegister circ) of
    Nothing -> Left $ "Wire not found: " ++ show wire
    Just _ -> do
      let op = createMeasurementOperation opid [WireId wire] time
      newCirc <- addOperationToCircuit op circ
      Right (newCirc, op)

-- | Add reset to circuit
addResetToCircuit :: OperationId -> Int -> Int -> Circuit -> Either String (Circuit, Operation)
addResetToCircuit opid time wire circ = do
  case getWire (WireId wire) (wireRegister circ) of
    Nothing -> Left $ "Wire not found: " ++ show wire
    Just _ -> do
      let op = createResetOperation opid [WireId wire] time
      newCirc <- addOperationToCircuit op circ
      Right (newCirc, op)

-- | Add barrier (global synchronization)
addBarrierToCircuit :: OperationId -> Circuit -> Either String (Circuit, Operation)
addBarrierToCircuit opid circ = do
  let allWires = map wireId (getAllWires (wireRegister circ))
  let time = operationCount (circuitDAG circ)
  let op = createBarrierOperation opid allWires time
  newCirc <- addOperationToCircuit op circ
  Right (newCirc, op)

-- | Connect dependencies between operations
connectOperations :: OperationId -> OperationId -> DependencyType -> Circuit -> Either String Circuit
connectOperations from to depType circ = do
  newDAG <- addDependency from to depType Nothing (circuitDAG circ)
  Right $ circ { circuitDAG = newDAG }

-- | Get circuit statistics
data CircuitStats = CircuitStats
  { statsQubitCount :: Int
  , statsGateCount :: Int
  , statsDepth :: Int
  , statsWidth :: Int
  , statsOperationCount :: Int
  , statsMeasurementCount :: Int
  , statsUnitaryCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON CircuitStats
instance FromJSON CircuitStats

-- | Compute circuit statistics
getCircuitStats :: Circuit -> Either String CircuitStats
getCircuitStats circ = do
  let qubits = registerSize (wireRegister circ)
  let gc = gateCount (gateLibrary circ)
  let dagOps = operationCount (circuitDAG circ)
  let (gates, meas, _, _) = countOperationsByType (circuitDAG circ)
  depth <- getCircuitDepth (circuitDAG circ)
  let width = getCircuitWidth (circuitDAG circ)

  Right $ CircuitStats
    { statsQubitCount = qubits
    , statsGateCount = gc
    , statsDepth = depth
    , statsWidth = width
    , statsOperationCount = dagOps
    , statsMeasurementCount = meas
    , statsUnitaryCount = gates
    }

-- | Validate complete circuit
validateCircuit :: Circuit -> Either String ()
validateCircuit circ = do
  -- Validate wire register
  case validateWireRegister (wireRegister circ) of
    Left err -> Left $ "Wire register validation failed: " ++ err
    Right () -> Right ()

  -- Validate DAG
  case validateDAG (circuitDAG circ) of
    Left err -> Left $ "DAG validation failed: " ++ err
    Right () -> Right ()

  -- Check all wires are used (if any operations)
  if operationCount (circuitDAG circ) > 0
    then do
      let allUsedWires = Set.fromList $ concatMap wireIds (Map.elems (operations (circuitDAG circ)))
      let registeredWires = Set.fromList $ map wireId (getAllWires (wireRegister circ))
      if Set.isSubsetOf allUsedWires registeredWires
        then Right ()
        else Left "Operations reference non-existent wires"
    else Right ()

-- | Export circuit to Quipper format (stub)
exportToQuipper :: Circuit -> String
exportToQuipper circ =
  unlines
    [ "-- Quipper circuit export"
    , "-- Circuit: " ++ show (circuitName circ)
    , "-- Qubits: " ++ show (statsQubitCount stats)
    , "-- Depth: " ++ show (statsDepth stats)
    , ""
    , "-- Operations:"
    ] ++ map opString (getAllOperations (circuitDAG circ))
  where
    stats = case getCircuitStats circ of
      Right s -> s
      Left _ -> CircuitStats 0 0 0 0 0 0 0

    opString op = "-- " ++ show (opId op) ++ ": " ++ show (opType op)

-- | Export circuit to OpenQASM format (stub)
exportToOpenQASM :: Circuit -> String
exportToOpenQASM circ =
  unlines
    [ "OPENQASM 2.0;"
    , "include \"qelib1.inc\";"
    , "qreg q[" ++ show (statsQubitCount stats) ++ "];"
    , ""
    , "// Operations:"
    ] ++ map opQASM (getAllOperations (circuitDAG circ))
  where
    stats = case getCircuitStats circ of
      Right s -> s
      Left _ -> CircuitStats 0 0 0 0 0 0 0

    opQASM op =
      case opType op of
        GateOperation -> case gate op of
          Just g -> "// gate " ++ show (gateId g)
          Nothing -> "// gate (unknown)"
        MeasurementOperation -> "measure q[0] -> c[0];"
        ResetOperation -> "reset q[0];"
        BarrierOperation -> "barrier q;"

-- | Circuit composition (concatenate two circuits)
-- Combines operations sequentially
composeCircuits :: Circuit -> Circuit -> Either String Circuit
composeCircuits c1 c2 = do
  -- Check compatible qubit counts
  let q1 = statsQubitCount (case getCircuitStats c1 of Right s -> s; Left _ -> CircuitStats 0 0 0 0 0 0 0)
  let q2 = statsQubitCount (case getCircuitStats c2 of Right s -> s; Left _ -> CircuitStats 0 0 0 0 0 0 0)

  if q1 /= q2
    then Left "Cannot compose circuits with different qubit counts"
    else do
      -- Merge DAGs (offset timestamps for c2)
      let c1Ops = getAllOperations (circuitDAG c1)
      let c2Ops = getAllOperations (circuitDAG c2)
      let timeOffset = if null c1Ops then 0 else maximum (map timestamp c1Ops) + 1

      -- Create new circuit with merged operations
      let mergedCirc = c1

      -- Add all operations from c2 with time offset
      foldM addOp mergedCirc (map (\o -> o { timestamp = timestamp o + timeOffset }) c2Ops)
  where
    addOp circ op = addOperationToCircuit op circ

-- | Identity circuit (no operations)
identityCircuit :: Int -> Circuit
identityCircuit nqubits = createCircuit (CircuitName "identity") nqubits

-- | Get circuit as string representation
circuitToString :: Circuit -> String
circuitToString circ = unlines $
  [ "Circuit: " ++ show (circuitName circ)
  , "Qubits: " ++ show (registerSize (wireRegister circ))
  , "Operations: " ++ show (operationCount (circuitDAG circ))
  ] ++
  case getCircuitStats circ of
    Left _ -> []
    Right stats ->
      [ "Depth: " ++ show (statsDepth stats)
      , "Width: " ++ show (statsWidth stats)
      , "Gates: " ++ show (statsUnitaryCount stats)
      , "Measurements: " ++ show (statsMeasurementCount stats)
      ]

-- | Helper: fold monadic operation
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a [] = return a
foldM f a (b:bs) = do
  a' <- f a b
  foldM f a' bs

-- | Helper: map monadic operation
forM_ :: [a] -> (a -> Either e b) -> Either e ()
forM_ [] _ = Right ()
forM_ (x:xs) f = do
  _ <- f x
  forM_ xs f
