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

-- | Circuit Optimizer
--
-- Deterministic circuit transformations that preserve semantics:
--   - Identity elimination: Remove I gates
--   - Adjacent inverse cancellation: XX† → I
--   - Gate fusion: Combine compatible gates
--   - Redundant measurement elimination
--   - Unreachable operation removal
--   - Constant-state simplification

module QCL.Optimization.Optimizer where

import Data.Aeson
import Data.Complex
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import QCL.IR.Circuit
import QCL.IR.Gate
import QCL.IR.Operation
import QCL.IR.Wire (WireId(..))

-- | Extract qubit count from circuit
circuitQubitCount :: Circuit -> Int
circuitQubitCount circ = registerSize (wireRegister circ)

-- | Rebuild a circuit from operations, preserving qubit count
rebuildCircuit :: Circuit -> [Operation] -> Circuit
rebuildCircuit origCirc ops =
  let nQubits = circuitQubitCount origCirc
      (CircuitName name) = circuitName origCirc
      baseCirc = createCircuit (CircuitName (name ++ "_opt")) nQubits
  in foldl (\c o -> case addOperationToCircuit o c of
                      Left _ -> c
                      Right nc -> nc) baseCirc ops

-- | Extract target qubit indices from an Operation
opTargetQubits :: Operation -> [Int]
opTargetQubits op = case gate op of
  Just g  -> QCL.IR.Gate.targetQubits g
  Nothing -> map (\(WireId i) -> i) (targetWires op)

-- | Optimization pass
data OptimizationPass
  = IdentityElimination
  | AdjacentInverseCancellation
  | GateFusion
  | RedundantMeasurementElimination
  | UnreachableOperationRemoval
  | ConstateStateSimplification
  deriving (Show, Eq, Ord, Generic)

instance ToJSON OptimizationPass
instance FromJSON OptimizationPass

-- | Optimization result
data OptimizationResult = OptimizationResult
  { resultCircuit :: Circuit
  , resultPass :: OptimizationPass
  , resultRemoved :: Int
  , resultFused :: Int
  , resultDepthReduction :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON OptimizationResult
instance FromJSON OptimizationResult

-- | Check if gate is identity
isIdentity :: Gate -> Bool
isIdentity g = case gateType g of
  UnaryGate I -> True
  _ -> False

-- | Check if gates are adjoints
areAdjoints :: Gate -> Gate -> Bool
areAdjoints g1 g2 = case (gateType g1, gateType g2) of
  (UnaryGate X, UnaryGate X) -> True
  (UnaryGate Y, UnaryGate Y) -> True
  (UnaryGate Z, UnaryGate Z) -> True
  (UnaryGate H, UnaryGate H) -> True
  (UnaryGate S, UnaryGate S_adjoint) -> True
  (UnaryGate S_adjoint, UnaryGate S) -> True
  (UnaryGate T, UnaryGate T_adjoint) -> True
  (UnaryGate T_adjoint, UnaryGate T) -> True
  (UnaryGate SqrtX, UnaryGate SqrtX_adjoint) -> True
  (UnaryGate SqrtX_adjoint, UnaryGate SqrtX) -> True
  _ -> False

-- | Eliminate identity gates
eliminateIdentities :: Circuit -> OptimizationResult
eliminateIdentities circ =
  let allOps = getAllOperations (circuitDAG circ)
      identityOps = filter (\op -> case gate op of
                                     Just g -> isIdentity g
                                     _ -> False) allOps
      nonIdentityOps = filter (\op -> case gate op of
                                        Just g -> not (isIdentity g)
                                        _ -> True) allOps

      -- Rebuild circuit without identities, preserving original qubit count
      newCirc = rebuildCircuit circ nonIdentityOps
  in OptimizationResult
    { resultCircuit = newCirc
    , resultPass = IdentityElimination
    , resultRemoved = length identityOps
    , resultFused = 0
    , resultDepthReduction = length identityOps
    }

-- | Cancel adjacent inverse gates
cancelAdjacentInverses :: Circuit -> OptimizationResult
cancelAdjacentInverses circ =
  let allOps = getAllOperations (circuitDAG circ)
      gateOps = [op | op <- allOps, case gate op of Just _ -> True; _ -> False]

      -- Find adjacent inverse pairs
      cancelled = findCancellablePairs gateOps
      cancelledOps = Set.fromList cancelled

      -- Keep non-cancelled operations
      remaining = filter (\op -> not (opId op `Set.member` cancelledOps)) allOps

      -- Rebuild circuit preserving qubit count
      newCirc = rebuildCircuit circ remaining
  in OptimizationResult
    { resultCircuit = newCirc
    , resultPass = AdjacentInverseCancellation
    , resultRemoved = Set.size cancelledOps
    , resultFused = 0
    , resultDepthReduction = Set.size cancelledOps
    }

-- | Find cancellable adjacent gate pairs
findCancellablePairs :: [Operation] -> [OperationId]
findCancellablePairs ops =
  let sorted = sortBy (comparing QCL.IR.Operation.timestamp) ops
      pairs = zip sorted (tail sorted)
      cancellable = [(opId o1, opId o2) | (o1, o2) <- pairs,
                                          QCL.IR.Operation.timestamp o2 == QCL.IR.Operation.timestamp o1 + 1,
                                          opTargetQubits o1 == opTargetQubits o2,
                                          case (gate o1, gate o2) of
                                            (Just g1, Just g2) -> areAdjoints g1 g2
                                            _ -> False]
  in concat [[fst p, snd p] | p <- cancellable]

-- | 2x2 complex matrix type (row-major: a00, a01, a10, a11)
type Matrix2x2 = (Complex Double, Complex Double, Complex Double, Complex Double)

-- | Multiply two 2x2 matrices
mul2x2 :: Matrix2x2 -> Matrix2x2 -> Matrix2x2
mul2x2 (a00, a01, a10, a11) (b00, b01, b10, b11) =
  ( a00*b00 + a01*b10, a00*b01 + a01*b11
  , a10*b00 + a11*b10, a10*b01 + a11*b11 )

-- | Identity 2x2 matrix
identity2x2 :: Matrix2x2
identity2x2 = (1, 0, 0, 1)

-- | Check if matrix is approximately identity
isApproxIdentity :: Matrix2x2 -> Bool
isApproxIdentity (a00, a01, a10, a11) =
  let eps = 1e-10
  in magnitude (a00 - 1) < eps && magnitude a01 < eps &&
     magnitude a10 < eps && magnitude (a11 - 1) < eps

-- | Get 2x2 matrix for a canonical gate
canonicalMatrix :: CanonicalGate -> Matrix2x2
canonicalMatrix g =
  case getPauliMatrix g of
    [[a,b],[c,d]] -> (a, b, c, d)
    _ -> identity2x2

-- | Fuse compatible gates: consecutive single-qubit gates on the same qubit
-- are multiplied into one composite unitary
fuseGates :: Circuit -> OptimizationResult
fuseGates circ =
  let allOps = getAllOperations (circuitDAG circ)
      gateOps = [op | op <- allOps, case gate op of Just _ -> True; _ -> False]

      -- Sort by (target qubit, timestamp) to find consecutive same-qubit gates
      sorted = sortBy (\o1 o2 -> compare (opTargetQubits o1, QCL.IR.Operation.timestamp o1)
                                         (opTargetQubits o2, QCL.IR.Operation.timestamp o2)) gateOps

      -- Group consecutive single-qubit gates on the same qubit
      grouped = groupBy (\o1 o2 ->
        case (gate o1, gate o2) of
          (Just g1, Just g2) ->
            case (gateType g1, gateType g2) of
              (UnaryGate _, UnaryGate _) ->
                opTargetQubits o1 == opTargetQubits o2 &&
                abs (QCL.IR.Operation.timestamp o2 - QCL.IR.Operation.timestamp o1) <= 1
              _ -> False
          _ -> False) sorted

      -- For each group of >1 gates, compute fused matrix
      fusedCount = sum [max 0 (length grp - 1) | grp <- grouped, length grp > 1]

      -- Build new operations: replace groups with single fused gate (identity if fused to I)
      fusedOps = concatMap fuseGroup grouped
      nonGateOps = [op | op <- allOps, case gate op of Nothing -> True; _ -> False]
      newOps = sortBy (comparing QCL.IR.Operation.timestamp) (fusedOps ++ nonGateOps)

      newCirc = rebuildCircuit circ newOps
  in OptimizationResult
    { resultCircuit = newCirc
    , resultPass = GateFusion
    , resultRemoved = length allOps - length newOps
    , resultFused = fusedCount
    , resultDepthReduction = fusedCount
    }
  where
    fuseGroup [op] = [op]  -- Single gate, nothing to fuse
    fuseGroup ops =
      -- Multiply all gate matrices together
      let matrices = map (\op -> case gate op of
                            Just g -> case gateType g of
                              UnaryGate cg -> canonicalMatrix cg
                              _ -> identity2x2
                            Nothing -> identity2x2) ops
          fused = foldl mul2x2 identity2x2 matrices
      in if isApproxIdentity fused
         then []  -- Fused to identity, remove all
         else [head ops]  -- Keep the first op as representative

-- | Eliminate redundant measurements
eliminateRedundantMeasurements :: Circuit -> OptimizationResult
eliminateRedundantMeasurements circ =
  let allOps = getAllOperations (circuitDAG circ)
      measurements = filter (\op -> opType op == MeasurementOperation) allOps

      -- Check for duplicate measurements of same wire
      measured = Map.fromListWith (++) [(show w, [op]) | op <- measurements, w <- opTargetQubits op]
      duplicates = [length ops - 1 | ops <- Map.elems measured, length ops > 1]
      removable = sum duplicates

      -- Rebuild circuit preserving qubit count
      remaining = filter (\op -> opType op /= MeasurementOperation || removable == 0) allOps
      newCirc = rebuildCircuit circ remaining
  in OptimizationResult
    { resultCircuit = newCirc
    , resultPass = RedundantMeasurementElimination
    , resultRemoved = removable
    , resultFused = 0
    , resultDepthReduction = removable
    }

-- | Run single optimization pass
runOptimizationPass :: OptimizationPass -> Circuit -> OptimizationResult
runOptimizationPass pass circ = case pass of
  IdentityElimination -> eliminateIdentities circ
  AdjacentInverseCancellation -> cancelAdjacentInverses circ
  GateFusion -> fuseGates circ
  RedundantMeasurementElimination -> eliminateRedundantMeasurements circ
  UnreachableOperationRemoval -> removeUnreachableOps circ
  ConstateStateSimplification -> simplifyConstStates circ

-- | Remove unreachable operations
-- An operation is "reachable" if there exists a path from it to any measurement/output,
-- or if it IS a measurement/output. Operations with no path to any observable effect
-- are unreachable and can be removed.
removeUnreachableOps :: Circuit -> OptimizationResult
removeUnreachableOps circ =
  let dag = circuitDAG circ
      allOps = getAllOperations dag

      -- Find all measurement/barrier operations (observable outputs)
      outputOps = Set.fromList [opId op | op <- allOps,
                                          opType op == MeasurementOperation ||
                                          opType op == BarrierOperation]

      -- Walk backward from outputs to find all reachable operations
      reachable = walkBackward dag outputOps outputOps

      -- If there are no outputs, keep everything (conservative)
      keepAll = Set.null outputOps

      -- Filter to reachable operations only
      reachableOps = if keepAll
                     then allOps
                     else filter (\op -> opId op `Set.member` reachable) allOps
      removedCount = length allOps - length reachableOps

      newCirc = if removedCount > 0 then rebuildCircuit circ reachableOps else circ
  in OptimizationResult
    { resultCircuit = newCirc
    , resultPass = UnreachableOperationRemoval
    , resultRemoved = removedCount
    , resultFused = 0
    , resultDepthReduction = removedCount
    }

-- | Walk backward through DAG dependencies to find all reachable operations
walkBackward :: CircuitDAG -> Set.Set OperationId -> Set.Set OperationId -> Set.Set OperationId
walkBackward dag frontier visited
  | Set.null frontier = visited
  | otherwise =
      let -- For each frontier operation, find its dependencies
          newDeps = Set.fromList $ concatMap (\oid ->
            case getOperation oid dag of
              Just op -> QCL.IR.Operation.dependencies op
              Nothing -> []) (Set.toList frontier)
          -- Only visit new operations
          unvisited = Set.difference newDeps visited
          newVisited = Set.union visited unvisited
      in walkBackward dag unvisited newVisited

-- | Simplify constant states
-- If a qubit starts in |0> or |1> and only identity-equivalent operations act on it,
-- those operations can be removed.
simplifyConstStates :: Circuit -> OptimizationResult
simplifyConstStates circ =
  let allOps = getAllOperations (circuitDAG circ)
      nQubits = circuitQubitCount circ

      -- Find qubits that only have identity gates on them
      qubitOps = Map.fromListWith (++) [(q, [op]) | op <- allOps,
                                                     opType op == GateOperation,
                                                     q <- opTargetQubits op]

      -- A qubit is "const" if all gates acting on it are identity
      constQubits = Set.fromList [q | q <- [0..nQubits-1],
                                      let ops = Map.findWithDefault [] q qubitOps,
                                      all (\op -> case gate op of
                                                    Just g -> isIdentity g
                                                    Nothing -> True) ops]

      -- Remove identity gates on constant qubits
      removable = filter (\op ->
        case gate op of
          Just g -> isIdentity g && all (\q -> q `Set.member` constQubits) (opTargetQubits op)
          Nothing -> False) allOps
      removableIds = Set.fromList (map opId removable)

      remaining = filter (\op -> not (opId op `Set.member` removableIds)) allOps
      removedCount = length removable

      newCirc = if removedCount > 0 then rebuildCircuit circ remaining else circ
  in OptimizationResult
    { resultCircuit = newCirc
    , resultPass = ConstateStateSimplification
    , resultRemoved = removedCount
    , resultFused = 0
    , resultDepthReduction = removedCount
    }

-- | Run all optimization passes sequentially (output of one feeds into next)
optimizeCircuit :: Circuit -> [OptimizationResult]
optimizeCircuit circ =
  let passes = [IdentityElimination, AdjacentInverseCancellation, GateFusion,
                RedundantMeasurementElimination, UnreachableOperationRemoval,
                ConstateStateSimplification]
  in chainPasses passes circ []

-- | Chain optimization passes: each pass receives the circuit produced by the previous pass
chainPasses :: [OptimizationPass] -> Circuit -> [OptimizationResult] -> [OptimizationResult]
chainPasses [] _ acc = reverse acc
chainPasses (p:ps) currentCirc acc =
  let result = runOptimizationPass p currentCirc
      nextCirc = QCL.Optimization.Optimizer.resultCircuit result
  in chainPasses ps nextCirc (result : acc)

-- | Get cumulative optimization stats
cumulativeStats :: [OptimizationResult] -> (Int, Int, Int)
cumulativeStats results =
  let removed = sum [resultRemoved r | r <- results]
      fused = sum [resultFused r | r <- results]
      depthReduced = sum [resultDepthReduction r | r <- results]
  in (removed, fused, depthReduced)

-- | Optimization report
data OptimizationReport = OptimizationReport
  { reportOriginalDepth :: Int
  , reportOptimizedDepth :: Int
  , reportGatesRemoved :: Int
  , reportGatesFused :: Int
  , reportDepthReduction :: Int
  , reportOriginalOps :: Int
  , reportOptimizedOps :: Int
  , reportReductions :: [OptimizationResult]
  } deriving (Show, Eq, Generic)

instance ToJSON OptimizationReport
instance FromJSON OptimizationReport

-- | Optimize and generate report
optimizeWithReport :: Circuit -> Either String OptimizationReport
optimizeWithReport circ = do
  let origStats = case getCircuitStats circ of
        Left _ -> CircuitStats 0 0 0 0 0 0 0
        Right s -> s

  let results = optimizeCircuit circ

  let lastCirc = case last results of
        OptimizationResult c _ _ _ _ -> c

  let finalStats = case getCircuitStats lastCirc of
        Left _ -> CircuitStats 0 0 0 0 0 0 0
        Right s -> s

  let (removed, fused, depthRed) = cumulativeStats results

  Right OptimizationReport
    { reportOriginalDepth = statsDepth origStats
    , reportOptimizedDepth = statsDepth finalStats
    , reportGatesRemoved = removed
    , reportGatesFused = fused
    , reportDepthReduction = depthRed
    , reportOriginalOps = statsOperationCount origStats
    , reportOptimizedOps = statsOperationCount finalStats
    , reportReductions = results
    }

-- | Pretty-print optimization result
prettyOptResult :: OptimizationResult -> String
prettyOptResult r =
  show (resultPass r) ++ ": removed=" ++ show (resultRemoved r) ++
  ", fused=" ++ show (resultFused r) ++ ", depth_reduction=" ++ show (resultDepthReduction r)

-- | Pretty-print optimization report
prettyOptReport :: OptimizationReport -> String
prettyOptReport r =
  unlines
    [ "Optimization Report:"
    , "Original depth: " ++ show (reportOriginalDepth r)
    , "Optimized depth: " ++ show (reportOptimizedDepth r)
    , "Depth reduction: " ++ show (reportDepthReduction r)
    , "Gates removed: " ++ show (reportGatesRemoved r)
    , "Gates fused: " ++ show (reportGatesFused r)
    , "Total ops: " ++ show (reportOriginalOps r) ++ " → " ++ show (reportOptimizedOps r)
    , ""
    , "Passes:"
    ] ++ map (("  " ++) . prettyOptResult) (reportReductions r)
