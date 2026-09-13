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

-- | Quantum Operation and Circuit IR (Intermediate Representation)
--
-- This module implements the directed acyclic dependency graph (DAG) for quantum circuits.
-- Every operation has explicit dependencies on prior operations, enabling:
--   - Topological ordering
--   - Dependency analysis
--   - Optimization (commutation, fusion)
--   - Parallel execution
--   - Resource allocation

module QCL.IR.Operation where

import Data.Aeson
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import QCL.IR.Wire
import QCL.IR.Gate

-- | Operation identifier (globally unique within circuit)
newtype OperationId = OperationId String
  deriving (Show, Eq, Ord, Generic)

instance ToJSON OperationId
instance FromJSON OperationId

-- | Type of quantum operation
data OperationType
  = GateOperation        -- ^ Unitary gate application
  | MeasurementOperation -- ^ Measurement
  | ResetOperation       -- ^ Reset/initialization
  | BarrierOperation     -- ^ Synchronization barrier
  deriving (Show, Eq, Ord, Generic)

instance ToJSON OperationType
instance FromJSON OperationType

-- | Dependency type
data DependencyType
  = DataDependency       -- ^ Read-after-write (RAW)
  | AntiDependency       -- ^ Write-after-read (WAR)
  | OutputDependency     -- ^ Write-after-write (WAW)
  | SynchronizationDep   -- ^ Explicit barrier synchronization
  deriving (Show, Eq, Ord, Generic)

instance ToJSON DependencyType
instance FromJSON DependencyType

-- | Dependency edge in DAG
data Dependency = Dependency
  { depFrom :: OperationId      -- ^ Producer operation
  , depTo :: OperationId        -- ^ Consumer operation
  , depType :: DependencyType
  , depWire :: Maybe WireId     -- ^ Which wire carries the dependency
  , depTimestamp :: Int         -- ^ Time step
  } deriving (Show, Eq, Generic)

instance ToJSON Dependency
instance FromJSON Dependency

-- | Complete quantum operation
data Operation = Operation
  { opId :: OperationId
  , opType :: OperationType
  , gate :: Maybe Gate                    -- ^ Gate (if gate operation)
  , wireIds :: [WireId]                   -- ^ Wires involved
  , controlWires :: [WireId]              -- ^ Control wires
  , targetWires :: [WireId]               -- ^ Target wires
  , dependencies :: [OperationId]         -- ^ Direct dependencies
  , dependents :: [OperationId]           -- ^ Operations depending on this
  , timestamp :: Int                      -- ^ Time step in circuit
  , classicalResult :: Maybe Int          -- ^ Classical bit result (if measurement)
  , duration :: Maybe Double              -- ^ Execution duration (seconds)
  , canCommute :: [OperationId]           -- ^ Operations this can commute with
  , sourceLocation :: Maybe (String, Int) -- ^ Source location
  } deriving (Show, Eq, Generic)

instance ToJSON Operation
instance FromJSON Operation

-- | Circuit DAG representation
data CircuitDAG = CircuitDAG
  { operations :: Map.Map OperationId Operation
  , operationCount :: Int
  , dagDependencies :: [Dependency]
  , adjacencyList :: Map.Map OperationId [OperationId]  -- ^ (opId → dependent opIds)
  , reverseAdjacency :: Map.Map OperationId [OperationId]  -- ^ (opId → dependencies)
  , topologicalOrder :: Maybe [OperationId]
  , criticalPath :: Maybe Double
  } deriving (Show, Eq, Generic)

instance ToJSON CircuitDAG
instance FromJSON CircuitDAG

-- | Create empty circuit DAG
emptyCircuitDAG :: CircuitDAG
emptyCircuitDAG = CircuitDAG
  { operations = Map.empty
  , operationCount = 0
  , dagDependencies = []
  , adjacencyList = Map.empty
  , reverseAdjacency = Map.empty
  , topologicalOrder = Nothing
  , criticalPath = Nothing
  }

-- | Create a gate operation
createGateOperation :: OperationId -> Gate -> [WireId] -> [WireId] -> Int -> Operation
createGateOperation opid g wires ctrlWires t =
  Operation
    { opId = opid
    , opType = GateOperation
    , gate = Just g
    , wireIds = wires
    , controlWires = ctrlWires
    , targetWires = filter (\w -> w `notElem` ctrlWires) wires
    , dependencies = []
    , dependents = []
    , timestamp = t
    , classicalResult = Nothing
    , duration = Nothing
    , canCommute = []
    , sourceLocation = Nothing
    }

-- | Create a measurement operation
createMeasurementOperation :: OperationId -> [WireId] -> Int -> Operation
createMeasurementOperation opid wires t =
  Operation
    { opId = opid
    , opType = MeasurementOperation
    , gate = Nothing
    , wireIds = wires
    , controlWires = []
    , targetWires = wires
    , dependencies = []
    , dependents = []
    , timestamp = t
    , classicalResult = Nothing
    , duration = Nothing
    , canCommute = []
    , sourceLocation = Nothing
    }

-- | Create a reset operation
createResetOperation :: OperationId -> [WireId] -> Int -> Operation
createResetOperation opid wires t =
  Operation
    { opId = opid
    , opType = ResetOperation
    , gate = Nothing
    , wireIds = wires
    , controlWires = []
    , targetWires = wires
    , dependencies = []
    , dependents = []
    , timestamp = t
    , classicalResult = Nothing
    , duration = Nothing
    , canCommute = []
    , sourceLocation = Nothing
    }

-- | Create a barrier operation
createBarrierOperation :: OperationId -> [WireId] -> Int -> Operation
createBarrierOperation opid wires t =
  Operation
    { opId = opid
    , opType = BarrierOperation
    , gate = Nothing
    , wireIds = wires
    , controlWires = []
    , targetWires = wires
    , dependencies = []
    , dependents = []
    , timestamp = t
    , classicalResult = Nothing
    , duration = Nothing
    , canCommute = []
    , sourceLocation = Nothing
    }

-- | Add operation to DAG
addOperation :: Operation -> CircuitDAG -> CircuitDAG
addOperation op dag =
  let newOps = Map.insert (opId op) op (operations dag)
  in dag
    { operations = newOps
    , operationCount = operationCount dag + 1
    , topologicalOrder = Nothing  -- Invalidate cached order
    }

-- | Add dependency between operations
addDependency :: OperationId -> OperationId -> DependencyType -> Maybe WireId -> CircuitDAG -> Either String CircuitDAG
addDependency fromOp toOp depType depWire dag = do
  -- Check both operations exist
  case (Map.lookup fromOp (operations dag), Map.lookup toOp (operations dag)) of
    (Nothing, _) -> Left $ "Source operation not found: " ++ show fromOp
    (_, Nothing) -> Left $ "Target operation not found: " ++ show toOp
    (Just from, Just to) -> do
      -- Update operations
      let updatedFrom = from { dependents = toOp : dependents from }
      let updatedTo = to { dependencies = fromOp : dependencies to }

      -- Update operations map
      let newOps = Map.insert fromOp updatedFrom (operations dag)
      let newOps' = Map.insert toOp updatedTo newOps

      -- Add to adjacency lists
      let newAdj = Map.insertWith (++) fromOp [toOp] (adjacencyList dag)
      let newRevAdj = Map.insertWith (++) toOp [fromOp] (reverseAdjacency dag)

      -- Add to dependency list
      let newDep = Dependency fromOp toOp depType depWire 0 : dagDependencies dag

      Right $ dag
        { operations = newOps'
        , dagDependencies = newDep
        , adjacencyList = newAdj
        , reverseAdjacency = newRevAdj
        , topologicalOrder = Nothing
        }

-- | Get operation by ID
getOperation :: OperationId -> CircuitDAG -> Maybe Operation
getOperation opid dag = Map.lookup opid (operations dag)

-- | Get all operations sorted by timestamp
getAllOperations :: CircuitDAG -> [Operation]
getAllOperations dag = sortBy (comparing timestamp) (Map.elems (operations dag))

-- | Compute topological order using Kahn's algorithm
topologicalSort :: CircuitDAG -> Either String [OperationId]
topologicalSort dag = do
  let ops = Map.elems (operations dag)
  let inDegree = Map.fromList [(opId op, length (dependencies op)) | op <- ops]

  -- Find all operations with in-degree 0
  let queue = [opId op | op <- ops, length (dependencies op) == 0]

  go queue Map.empty inDegree []
  where
    go [] _ _ result = Right (reverse result)
    go (n:queue) visited inDeg result = do
      case Map.lookup n (operations dag) of
        Nothing -> Left $ "Operation not found: " ++ show n
        Just op -> do
          -- Get dependent operations
          let deps = Map.findWithDefault [] n (adjacencyList dag)
          -- Decrement in-degree for each dependent
          let (newInDeg, newQueue) = foldl decrement (inDeg, queue) deps
          go newQueue visited newInDeg (n : result)

    decrement (inDeg, queue) depId =
      let newDeg = Map.adjust (\d -> d - 1) depId inDeg
          degreeNow = Map.findWithDefault 0 depId newDeg
      in if degreeNow == 0
         then (newDeg, queue ++ [depId])
         else (newDeg, queue)

-- | Check if DAG is acyclic
isAcyclic :: CircuitDAG -> Bool
isAcyclic dag = case topologicalSort dag of
  Right order -> length order == operationCount dag
  Left _ -> False

-- | Get critical path (longest path through DAG)
-- Simplified: assumes uniform operation duration
computeCriticalPath :: CircuitDAG -> Either String Double
computeCriticalPath dag = do
  order <- topologicalSort dag
  -- For each operation, compute longest path to it
  let pathLengths = foldl updatePath Map.empty order
  case Map.elems pathLengths of
    [] -> Right 0
    lengths -> Right (maximum lengths)
  where
    updatePath pathMap opid =
      let op = case Map.lookup opid (operations dag) of
            Just o -> o
            Nothing -> error "Operation not found"
          deps = dependencies op
          depLengths = [Map.findWithDefault 0 dep pathMap | dep <- deps]
          myLength = case depLengths of
                       [] -> 1  -- No dependencies
                       l -> maximum l + 1
      in Map.insert opid (fromIntegral myLength) pathMap

-- | Find operations on same wire
operationsOnWire :: WireId -> CircuitDAG -> [Operation]
operationsOnWire wire dag =
  filter (\op -> wire `elem` wireIds op) (Map.elems (operations dag))

-- | Find last operation on wire
lastOperationOnWire :: WireId -> CircuitDAG -> Maybe Operation
lastOperationOnWire wire dag =
  let ops = operationsOnWire wire dag
  in case ops of
    [] -> Nothing
    _ -> Just $ last (sortBy (comparing timestamp) ops)

-- | Validate DAG (acyclic, all dependencies exist)
validateDAG :: CircuitDAG -> Either String ()
validateDAG dag = do
  -- Check acyclic
  if not (isAcyclic dag)
    then Left "Circuit DAG contains cycles"
    else Right ()

  -- Check all dependencies reference existing operations
  let allOpIds = Set.fromList (Map.keys (operations dag))
  let deps = Set.fromList $ concatMap (\d -> [depFrom d, depTo d]) (dagDependencies dag)
  if not (Set.isSubsetOf deps allOpIds)
    then Left "Dependency references non-existent operation"
    else Right ()

  -- Check no self-dependencies
  forM_ (dagDependencies dag) $ \dep ->
    if depFrom dep == depTo dep
      then Left "Operation has self-dependency"
      else Right ()

  Right ()

-- | Serialize DAG to DOT format for visualization
serializeToDot :: CircuitDAG -> String
serializeToDot dag =
  unlines $
    ["digraph CircuitDAG {"]
    ++ ["  rankdir=LR"]
    ++ map operationNode (Map.elems (operations dag))
    ++ map dependencyEdge (dagDependencies dag)
    ++ ["}"]
  where
    operationNode op =
      let label = case opType op of
            GateOperation -> "Gate"
            MeasurementOperation -> "Measure"
            ResetOperation -> "Reset"
            BarrierOperation -> "Barrier"
          (OperationId nodeId) = opId op
      in "  " ++ nodeId ++ " [label=\"" ++ label ++ " @" ++ show (timestamp op) ++ "\"]"

    dependencyEdge dep =
      let (OperationId fromId) = depFrom dep
          (OperationId toId) = depTo dep
          label = show (depType dep)
      in "  " ++ fromId ++ " -> " ++ toId ++ " [label=\"" ++ label ++ "\"]"

-- | Helper: map monadic operation over list
forM_ :: [a] -> (a -> Either e b) -> Either e ()
forM_ [] _ = Right ()
forM_ (x:xs) f = do
  _ <- f x
  forM_ xs f

-- | Count operations by type
countOperationsByType :: CircuitDAG -> (Int, Int, Int, Int)
countOperationsByType dag =
  let ops = Map.elems (operations dag)
      gates = length $ filter (\o -> opType o == GateOperation) ops
      meas = length $ filter (\o -> opType o == MeasurementOperation) ops
      reset = length $ filter (\o -> opType o == ResetOperation) ops
      barrier = length $ filter (\o -> opType o == BarrierOperation) ops
  in (gates, meas, reset, barrier)

-- | Get depth of circuit (longest path from input to output)
getCircuitDepth :: CircuitDAG -> Either String Int
getCircuitDepth dag = do
  path <- computeCriticalPath dag
  Right (ceiling path)

-- | Get width of circuit (maximum number of simultaneous wires)
getCircuitWidth :: CircuitDAG -> Int
getCircuitWidth dag =
  case Map.elems (operations dag) of
    [] -> 0
    ops -> length $ Set.fromList $ concatMap wireIds ops
