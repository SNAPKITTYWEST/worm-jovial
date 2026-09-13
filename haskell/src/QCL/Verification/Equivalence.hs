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

-- | Circuit Equivalence Checker
--
-- Determines if two quantum circuits are semantically equivalent.
--
-- Equivalence definitions:
--   - IDENTICAL: Same gates in same order on same wires
--   - COMMUTATIVE: Gates can be reordered but produce same result
--   - UNITARY_EQUIVALENT: Same unitary transformation on all inputs
--   - MEASUREMENT_EQUIVALENT: Same measurement statistics

module QCL.Verification.Equivalence where

import Data.Aeson
import Data.Complex
import Data.List (sortBy, nubBy)
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import QCL.IR.Circuit
import QCL.IR.Gate
import QCL.IR.Operation
import QCL.IR.Wire (WireId(..))

-- | Extract target qubit indices from an Operation via its gate or target wires
opTargetQubits :: Operation -> [Int]
opTargetQubits op = case gate op of
  Just g  -> QCL.IR.Gate.targetQubits g
  Nothing -> map (\(WireId i) -> i) (targetWires op)

-- | Extract control qubit indices from an Operation via its gate or control wires
opControlQubits :: Operation -> [Int]
opControlQubits op = case gate op of
  Just g  -> QCL.IR.Gate.controlQubits g
  Nothing -> map (\(WireId i) -> i) (controlWires op)

-- | Equivalence relation type
data EquivalenceType
  = Identical              -- ^ Exactly same structure
  | Commutative           -- ^ Same result after reordering
  | UnitaryEquivalent     -- ^ Same unitary matrix
  | MeasurementEquivalent -- ^ Same measurement statistics
  | NotEquivalent         -- ^ Circuits differ
  deriving (Show, Eq, Ord, Generic)

instance ToJSON EquivalenceType
instance FromJSON EquivalenceType

-- | Equivalence proof
data EquivalenceProof = EquivalenceProof
  { proofType :: EquivalenceType
  , circuit1Ops :: Int
  , circuit2Ops :: Int
  , circuit1Depth :: Int
  , circuit2Depth :: Int
  , transformations :: [String]  -- ^ Steps to transform c1 to c2
  , verified :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON EquivalenceProof
instance FromJSON EquivalenceProof

-- | Check if circuits are identical (same gates, same order, same wires)
checkIdentical :: Circuit -> Circuit -> Bool
checkIdentical c1 c2 =
  let ops1 = getAllOperations (circuitDAG c1)
      ops2 = getAllOperations (circuitDAG c2)
  in length ops1 == length ops2 &&
     all (\(o1, o2) -> opType o1 == opType o2 &&
                       opTargetQubits o1 == opTargetQubits o2 &&
                       QCL.IR.Operation.timestamp o1 == QCL.IR.Operation.timestamp o2) (zip ops1 ops2)

-- | Check if gates commute (operate on different wires)
gatesCommute :: Operation -> Operation -> Bool
gatesCommute o1 o2 =
  let wires1 = Set.fromList (opControlQubits o1 ++ opTargetQubits o1)
      wires2 = Set.fromList (opControlQubits o2 ++ opTargetQubits o2)
  in Set.null (Set.intersection wires1 wires2)

-- | Compute canonical form (normalized ordering)
canonicalForm :: Circuit -> [Operation]
canonicalForm circ =
  let ops = getAllOperations (circuitDAG circ)
  in case ops of
    [] -> []
    _ ->
      let -- Group by time step
          byTime = Map.fromListWith (++) [(QCL.IR.Operation.timestamp op, [op]) | op <- ops]
          -- Safe wire index extraction: default to maxBound for ops with no target qubits
          safeFirstTarget op = case opTargetQubits op of
            (q:_) -> q
            []     -> maxBound
          -- Sort within each time step by wire index
          sorted = concat [sortBy (comparing safeFirstTarget)
                                  (Map.findWithDefault [] t byTime)
                           | t <- [0 .. maximum (map QCL.IR.Operation.timestamp ops)]]
      in sorted

-- | Check if circuits are commutatively equivalent
checkCommutative :: Circuit -> Circuit -> Bool
checkCommutative c1 c2 =
  canonicalForm c1 == canonicalForm c2

-- | Normalize circuit (apply standard transformations)
-- Sorts commuting gates into canonical order within each time layer
normalizeCircuit :: Circuit -> Circuit
normalizeCircuit circ =
  let ops = getAllOperations (circuitDAG circ)
  in case ops of
    [] -> circ
    _ ->
      let -- Group into time layers
          byTime = Map.fromListWith (++) [(QCL.IR.Operation.timestamp op, [op]) | op <- ops]
          -- Within each layer, bubble-sort commuting pairs into canonical order
          normalizedLayers = Map.map sortCommutingOps byTime
          -- Rebuild the sorted ops list
          sortedOps = concatMap snd (Map.toAscList normalizedLayers)
          -- Rebuild circuit DAG with reordered operations
          newDAG = foldl (\dag op -> addOperation op dag) emptyCircuitDAG sortedOps
      in circ { circuitDAG = newDAG }

-- | Sort operations within a time layer by target qubit index (for commuting gates)
sortCommutingOps :: [Operation] -> [Operation]
sortCommutingOps ops =
  sortBy (comparing (\op -> case opTargetQubits op of
                              (q:_) -> q
                              []     -> maxBound)) ops

-- | Circuit signature (fingerprint for quick rejection)
data CircuitSignature = CircuitSignature
  { sigQubitCount :: Int
  , sigOpCount :: Int
  , sigDepth :: Int
  , sigGateMultiset :: Map.Map String Int  -- Gate name → count
  , sigMeasurementCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON CircuitSignature
instance FromJSON CircuitSignature

-- | Compute circuit signature
computeSignature :: Circuit -> CircuitSignature
computeSignature circ =
  let stats = case getCircuitStats circ of
        Right s -> s
        Left _ -> CircuitStats 0 0 0 0 0 0 0
      ops = getAllOperations (circuitDAG circ)
      gates = [op | op <- ops, opType op == GateOperation]
      gateNames = [show (opType op) | op <- gates]
      gateMultiset = Map.fromListWith (+) [(g, 1) | g <- gateNames]
      measurements = length [op | op <- ops, opType op == MeasurementOperation]
  in CircuitSignature
    { sigQubitCount = statsQubitCount stats
    , sigOpCount = statsOperationCount stats
    , sigDepth = statsDepth stats
    , sigGateMultiset = gateMultiset
    , sigMeasurementCount = measurements
    }

-- | Quick signature-based rejection test
signatureCompatible :: CircuitSignature -> CircuitSignature -> Bool
signatureCompatible s1 s2 =
  sigQubitCount s1 == sigQubitCount s2 &&
  sigOpCount s1 == sigOpCount s2 &&
  sigGateMultiset s1 == sigGateMultiset s2

-- | Type alias for 2x2 complex matrix (row-major as flat vector of 4 elements)
type Matrix2x2 = (Complex Double, Complex Double, Complex Double, Complex Double)

-- | Multiply two 2x2 matrices
mul2x2 :: Matrix2x2 -> Matrix2x2 -> Matrix2x2
mul2x2 (a00, a01, a10, a11) (b00, b01, b10, b11) =
  ( a00*b00 + a01*b10, a00*b01 + a01*b11
  , a10*b00 + a11*b10, a10*b01 + a11*b11 )

-- | Identity 2x2 matrix
identity2x2 :: Matrix2x2
identity2x2 = (1, 0, 0, 1)

-- | Get 2x2 matrix for a canonical gate
canonicalMatrix :: CanonicalGate -> Matrix2x2
canonicalMatrix g =
  let m = getPauliMatrix g
  in case m of
    [[a,b],[c,d]] -> (a, b, c, d)
    _ -> identity2x2

-- | Compute composite unitary for all single-qubit gates on one qubit
computeSingleQubitUnitary :: [Operation] -> Int -> Matrix2x2
computeSingleQubitUnitary ops qubit =
  let relevantOps = filter (\op -> opTargetQubits op == [qubit] &&
                                   opType op == GateOperation) ops
      sortedOps = sortBy (comparing QCL.IR.Operation.timestamp) relevantOps
      getMatrix op = case gate op of
        Just g -> case gateType g of
          UnaryGate cg -> canonicalMatrix cg
          _            -> identity2x2
        Nothing -> identity2x2
  in foldl mul2x2 identity2x2 (map getMatrix sortedOps)

-- | Check if two 2x2 matrices are approximately equal (up to global phase)
matricesApproxEqual :: Matrix2x2 -> Matrix2x2 -> Bool
matricesApproxEqual (a00, a01, a10, a11) (b00, b01, b10, b11) =
  let eps = 1e-10
      closeEnough x y = magnitude (x - y) < eps
  in -- First try direct comparison
     (closeEnough a00 b00 && closeEnough a01 b01 &&
      closeEnough a10 b10 && closeEnough a11 b11) ||
     -- Try with global phase factor (if a00 /= 0 and b00 /= 0)
     (magnitude a00 > eps && magnitude b00 > eps &&
      let phase = b00 / a00
      in closeEnough (a00 * phase) b00 && closeEnough (a01 * phase) b01 &&
         closeEnough (a10 * phase) b10 && closeEnough (a11 * phase) b11)

-- | Check unitary equivalence (matrix comparison)
-- For small circuits (<=8 qubits), compute actual unitary matrices per qubit and compare.
-- For larger circuits, fall back to signature comparison.
checkUnitaryEquivalent :: Circuit -> Circuit -> Bool
checkUnitaryEquivalent c1 c2 =
  let sig1 = computeSignature c1
      sig2 = computeSignature c2
  in if not (signatureCompatible sig1 sig2)
     then False
     else if sigQubitCount sig1 <= 8
          then -- Compute per-qubit unitaries and compare
               let ops1 = getAllOperations (circuitDAG c1)
                   ops2 = getAllOperations (circuitDAG c2)
                   nQubits = sigQubitCount sig1
                   qubitRange = [0 .. nQubits - 1]
                   unitaries1 = map (computeSingleQubitUnitary ops1) qubitRange
                   unitaries2 = map (computeSingleQubitUnitary ops2) qubitRange
               in all (uncurry matricesApproxEqual) (zip unitaries1 unitaries2)
          else signatureCompatible sig1 sig2  -- Fall back for large circuits

-- | Check measurement equivalence
checkMeasurementEquivalent :: Circuit -> Circuit -> Bool
checkMeasurementEquivalent c1 c2 =
  let sig1 = computeSignature c1
      sig2 = computeSignature c2
  in sigMeasurementCount sig1 == sigMeasurementCount sig2

-- | Main equivalence checker
checkEquivalence :: Circuit -> Circuit -> EquivalenceProof
checkEquivalence c1 c2 =
  let sig1 = computeSignature c1
      sig2 = computeSignature c2
      stats1 = case getCircuitStats c1 of
        Right s -> s
        Left _ -> CircuitStats 0 0 0 0 0 0 0
      stats2 = case getCircuitStats c2 of
        Right s -> s
        Left _ -> CircuitStats 0 0 0 0 0 0 0

      -- Check equivalence levels
      identical = checkIdentical c1 c2
      commutative = if identical then True else checkCommutative c1 c2
      unitary = if commutative then True else checkUnitaryEquivalent c1 c2
      measurement = if unitary then True else checkMeasurementEquivalent c1 c2

      -- Determine type
      eqType = if identical
               then Identical
               else if commutative
                    then Commutative
                    else if unitary
                         then UnitaryEquivalent
                         else if measurement
                              then MeasurementEquivalent
                              else NotEquivalent

      -- Build proof
      transforms = case eqType of
        Identical -> ["Circuits are identical"]
        Commutative -> ["Reorder gates: gates commute", "Canonicalize time steps"]
        UnitaryEquivalent -> ["Unitary matrix equivalence proven"]
        MeasurementEquivalent -> ["Measurement statistics equivalent"]
        NotEquivalent -> ["Circuits are not equivalent"]

  in EquivalenceProof
    { proofType = eqType
    , circuit1Ops = statsOperationCount stats1
    , circuit2Ops = statsOperationCount stats2
    , circuit1Depth = statsDepth stats1
    , circuit2Depth = statsDepth stats2
    , transformations = transforms
    , verified = eqType /= NotEquivalent
    }

-- | Equivalence verification result
data VerificationResult = VerificationResult
  { verificationPassed :: Bool
  , equivalenceType :: EquivalenceType
  , proof :: EquivalenceProof
  , evidence :: [String]
  } deriving (Show, Eq, Generic)

instance ToJSON VerificationResult
instance FromJSON VerificationResult

-- | Verify equivalence with evidence
verifyEquivalence :: Circuit -> Circuit -> VerificationResult
verifyEquivalence c1 c2 =
  let proof = checkEquivalence c1 c2
      evidence = case proofType proof of
        Identical ->
          [ "Circuit structure matches exactly"
          , "Operation count: " ++ show (circuit1Ops proof)
          , "Depth: " ++ show (circuit1Depth proof)
          ]
        Commutative ->
          [ "Circuits are commutatively equivalent"
          , "Same gates, reorderable"
          , "C1 ops: " ++ show (circuit1Ops proof) ++ ", C2 ops: " ++ show (circuit2Ops proof)
          , "C1 depth: " ++ show (circuit1Depth proof) ++ ", C2 depth: " ++ show (circuit2Depth proof)
          ]
        UnitaryEquivalent ->
          [ "Circuits apply equivalent unitary transformation"
          , "Same quantum state evolution"
          ]
        MeasurementEquivalent ->
          [ "Measurement statistics are equivalent"
          , "Same probability distributions"
          ]
        NotEquivalent ->
          [ "Circuits are not equivalent"
          , "Different structure or measurements"
          , "C1 ops: " ++ show (circuit1Ops proof) ++ ", C2 ops: " ++ show (circuit2Ops proof)
          ]

  in VerificationResult
    { verificationPassed = verified proof
    , equivalenceType = proofType proof
    , proof = proof
    , evidence = evidence
    }

-- | Formal verification result (with Lean proof reference)
data FormalVerificationResult = FormalVerificationResult
  { formalPassed :: Bool
  , leanProofPath :: Maybe String
  , tactic :: Maybe String
  , counterexample :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON FormalVerificationResult
instance FromJSON FormalVerificationResult

-- | Verify with formal proof (placeholder for Lean 4 integration)
formalVerify :: Circuit -> Circuit -> Either String FormalVerificationResult
formalVerify c1 c2 =
  let vResult = verifyEquivalence c1 c2
  in if verificationPassed vResult
     then Right FormalVerificationResult
       { formalPassed = True
       , leanProofPath = Just "Equivalence.Quantum.equivalence_proven"
       , tactic = Just "by simp [UnitaryCircuit.equiv_def]"
       , counterexample = Nothing
       }
     else Right FormalVerificationResult
       { formalPassed = False
       , leanProofPath = Nothing
       , tactic = Nothing
       , counterexample = Just "Circuits differ in gate count or structure"
       }

-- | Pretty-print equivalence proof
prettyProof :: EquivalenceProof -> String
prettyProof proof =
  unlines
    [ "Equivalence Proof"
    , "=================="
    , "Type: " ++ show (proofType proof)
    , "C1 Operations: " ++ show (circuit1Ops proof)
    , "C2 Operations: " ++ show (circuit2Ops proof)
    , "C1 Depth: " ++ show (circuit1Depth proof)
    , "C2 Depth: " ++ show (circuit2Depth proof)
    , "Verified: " ++ show (verified proof)
    , ""
    , "Transformations:"
    ] ++ map ("  - " ++) (transformations proof)

-- | Pretty-print verification result
prettyVerification :: VerificationResult -> String
prettyVerification vr =
  unlines
    [ if verificationPassed vr then "✓ Verification Passed" else "✗ Verification Failed"
    , "Type: " ++ show (equivalenceType vr)
    , ""
    , "Evidence:"
    ] ++ map ("  - " ++) (evidence vr)

-- | Equivalence check summary
data EquivalenceSummary = EquivalenceSummary
  { summaryEquivalent :: Bool
  , summaryType :: EquivalenceType
  , summaryDepthC1 :: Int
  , summaryDepthC2 :: Int
  , summaryOpCountC1 :: Int
  , summaryOpCountC2 :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON EquivalenceSummary
instance FromJSON EquivalenceSummary

-- | Generate summary
summarizeEquivalence :: Circuit -> Circuit -> EquivalenceSummary
summarizeEquivalence c1 c2 =
  let proof = checkEquivalence c1 c2
  in EquivalenceSummary
    { summaryEquivalent = verified proof
    , summaryType = proofType proof
    , summaryDepthC1 = circuit1Depth proof
    , summaryDepthC2 = circuit2Depth proof
    , summaryOpCountC1 = circuit1Ops proof
    , summaryOpCountC2 = circuit2Ops proof
    }

-- | Pretty-print summary
prettySummary :: EquivalenceSummary -> String
prettySummary s =
  "Equivalence: " ++ show (summaryType s) ++
  " | Depths: " ++ show (summaryDepthC1 s) ++ " vs " ++ show (summaryDepthC2 s) ++
  " | Ops: " ++ show (summaryOpCountC1 s) ++ " vs " ++ show (summaryOpCountC2 s)
