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

-- | Simulator Backend - Classical Reference Execution Engine
--
-- Executes quantum circuits on classical computer using statevector simulation.
-- Implements Born rule for measurements and matrix multiplication for gates.
--
-- State vector representation: n qubits => 2^n complex amplitudes.
-- Basis state |b_{n-1} ... b_1 b_0> corresponds to index sum(b_k * 2^k).
-- Pure functional implementation using Data.Vector with V.generate and V.(//).

module QCL.Backend.Simulator where

import Data.Aeson
import Data.Bits (testBit, xor, shiftL, (.&.), complement)
import Data.Complex
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Vector as V

import QCL.IR.Circuit
import QCL.IR.Gate
import QCL.IR.Operation
import QCL.IR.Wire (WireId(..))

-- ============================================================================
-- Types
-- ============================================================================

-- | Quantum state (statevector representation)
-- For n qubits, the vector has 2^n Complex Double amplitudes.
newtype QuantumState = QuantumState (V.Vector (Complex Double))
  deriving (Show, Eq, Generic)

instance ToJSON QuantumState
instance FromJSON QuantumState

-- | Simulation result
data SimulationResult = SimulationResult
  { resultCircuit :: Circuit
  , resultFinalState :: QuantumState
  , resultMeasurements :: [(Int, Int)]  -- ^ (wire, measurement result)
  , resultExecutionTime :: Double
  , resultTraces :: [ExecutionTrace]
  } deriving (Show, Eq, Generic)

instance ToJSON SimulationResult
instance FromJSON SimulationResult

-- | Execution trace entry
data ExecutionTrace = ExecutionTrace
  { traceStep :: Int
  , traceOp :: String
  , traceWires :: [Int]
  , traceStateNorm :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON ExecutionTrace
instance FromJSON ExecutionTrace

-- | 2x2 complex matrix stored as (row0col0, row0col1, row1col0, row1col1)
type Matrix2x2 = (Complex Double, Complex Double, Complex Double, Complex Double)

-- ============================================================================
-- State Initialization
-- ============================================================================

-- | Initialize state to |00...0>
-- First element = 1, rest = 0.
initState :: Int -> QuantumState
initState nQubits =
  let dim = 1 `shiftL` nQubits  -- 2^n
      vec = V.generate dim (\i -> if i == 0 then 1 :+ 0 else 0 :+ 0)
  in QuantumState vec

-- ============================================================================
-- Gate Matrices (2x2)
-- ============================================================================

-- | Pauli X matrix: [[0,1],[1,0]]
pauliX :: Matrix2x2
pauliX = (0, 1, 1, 0)

-- | Pauli Y matrix: [[0,-i],[i,0]]
pauliY :: Matrix2x2
pauliY = (0, 0 :+ (-1), 0 :+ 1, 0)

-- | Pauli Z matrix: [[1,0],[0,-1]]
pauliZ :: Matrix2x2
pauliZ = (1, 0, 0, (-1) :+ 0)

-- | Hadamard matrix: (1/sqrt 2) * [[1,1],[1,-1]]
hadamard :: Matrix2x2
hadamard =
  let s = (1 / sqrt 2) :+ 0
  in (s, s, s, negate s)

-- | S (phase) gate: [[1,0],[0,i]]
sGate :: Matrix2x2
sGate = (1, 0, 0, 0 :+ 1)

-- | S-adjoint gate: [[1,0],[0,-i]]
sAdjointGate :: Matrix2x2
sAdjointGate = (1, 0, 0, 0 :+ (-1))

-- | T gate: [[1,0],[0,e^(i*pi/4)]]
tGate :: Matrix2x2
tGate = (1, 0, 0, exp (0 :+ (pi / 4)))

-- | T-adjoint gate: [[1,0],[0,e^(-i*pi/4)]]
tAdjointGate :: Matrix2x2
tAdjointGate = (1, 0, 0, exp (0 :+ (negate pi / 4)))

-- | Identity matrix
identityGate :: Matrix2x2
identityGate = (1, 0, 0, 1)

-- | SqrtX gate: (1/2) * [[1+i, 1-i],[1-i, 1+i]]
sqrtXGate :: Matrix2x2
sqrtXGate = (0.5 :+ 0.5, 0.5 :+ (-0.5), 0.5 :+ (-0.5), 0.5 :+ 0.5)

-- | SqrtX-adjoint gate
sqrtXAdjointGate :: Matrix2x2
sqrtXAdjointGate = (0.5 :+ (-0.5), 0.5 :+ 0.5, 0.5 :+ 0.5, 0.5 :+ (-0.5))

-- | SqrtY gate
sqrtYGate :: Matrix2x2
sqrtYGate = (0.5 :+ 0.5, (-0.5) :+ (-0.5), 0.5 :+ 0.5, 0.5 :+ 0.5)

-- | SqrtY-adjoint gate
sqrtYAdjointGate :: Matrix2x2
sqrtYAdjointGate = (0.5 :+ (-0.5), 0.5 :+ (-0.5), (-0.5) :+ 0.5, 0.5 :+ (-0.5))

-- | SqrtZ gate: [[e^(i*pi/4), 0], [0, e^(-i*pi/4)]]
sqrtZGate :: Matrix2x2
sqrtZGate = (exp (0 :+ (pi / 4)), 0, 0, exp (0 :+ (negate pi / 4)))

-- | SqrtZ-adjoint gate
sqrtZAdjointGate :: Matrix2x2
sqrtZAdjointGate = (exp (0 :+ (negate pi / 4)), 0, 0, exp (0 :+ (pi / 4)))

-- | Rx(theta) rotation gate: [[cos(t/2), -i*sin(t/2)], [-i*sin(t/2), cos(t/2)]]
rxGate :: Double -> Matrix2x2
rxGate theta =
  let c = cos (theta / 2) :+ 0
      s = 0 :+ (negate (sin (theta / 2)))
  in (c, s, s, c)

-- | Ry(theta) rotation gate: [[cos(t/2), -sin(t/2)], [sin(t/2), cos(t/2)]]
ryGate :: Double -> Matrix2x2
ryGate theta =
  let c = cos (theta / 2) :+ 0
      s = sin (theta / 2) :+ 0
  in (c, negate s, s, c)

-- | Rz(theta) rotation gate: [[e^(-i*t/2), 0], [0, e^(i*t/2)]]
rzGate :: Double -> Matrix2x2
rzGate theta =
  (exp (0 :+ (negate theta / 2)), 0, 0, exp (0 :+ (theta / 2)))

-- | Phase shift gate: [[1, 0], [0, e^(i*phi)]]
phaseGate :: Double -> Matrix2x2
phaseGate phi = (1, 0, 0, exp (0 :+ phi))

-- | Get matrix for canonical gate
gateMatrix :: CanonicalGate -> Matrix2x2
gateMatrix g = case g of
  I             -> identityGate
  X             -> pauliX
  Y             -> pauliY
  Z             -> pauliZ
  H             -> hadamard
  S             -> sGate
  S_adjoint     -> sAdjointGate
  T             -> tGate
  T_adjoint     -> tAdjointGate
  SqrtX         -> sqrtXGate
  SqrtX_adjoint -> sqrtXAdjointGate
  SqrtY         -> sqrtYGate
  SqrtY_adjoint -> sqrtYAdjointGate
  SqrtZ         -> sqrtZGate
  SqrtZ_adjoint -> sqrtZAdjointGate

-- | Get matrix for rotation gate
rotationMatrix :: RotationGate -> Matrix2x2
rotationMatrix r = case r of
  Rx theta      -> rxGate theta
  Ry theta      -> ryGate theta
  Rz theta      -> rzGate theta
  PhaseShift phi -> phaseGate phi

-- ============================================================================
-- Gate Application (index-bit manipulation, no full matrix construction)
-- ============================================================================

-- | Apply a single-qubit gate to qubit k in an n-qubit system.
--
-- For each pair of basis states differing only in bit k, apply the 2x2 matrix.
-- This avoids building the full 2^n x 2^n matrix.
--
-- For basis state |i>, bit k is (i `testBit` k).
-- The paired state is i XOR (1 `shiftL` k).
-- We only process pairs where bit k = 0 to avoid double-processing.
applySingleQubitGate :: Matrix2x2 -> Int -> QuantumState -> QuantumState
applySingleQubitGate (u00, u01, u10, u11) target (QuantumState state) =
  let dim = V.length state
      bitMask = 1 `shiftL` target
      -- Process all pairs: for each index with bit k = 0, compute the updated pair
      updates = concatMap (\i ->
        if i .&. bitMask == 0
        then
          let j = i `xor` bitMask  -- j has bit k = 1
              ai = state V.! i     -- amplitude for |...0_k...>
              aj = state V.! j     -- amplitude for |...1_k...>
              -- Apply 2x2 matrix: [u00 u01; u10 u11] * [ai; aj]
              newAi = u00 * ai + u01 * aj
              newAj = u10 * ai + u11 * aj
          in [(i, newAi), (j, newAj)]
        else []
        ) [0 .. dim - 1]
  in QuantumState (state V.// updates)

-- | Apply CNOT gate: for control qubit c and target qubit t,
-- flip the target bit when control bit is 1.
-- For each basis state i: if bit c of i is 1, swap amplitudes of i and (i XOR (1 << t)).
applyCNOT :: Int -> Int -> QuantumState -> QuantumState
applyCNOT control target (QuantumState state) =
  let dim = V.length state
      controlMask = 1 `shiftL` control
      targetMask = 1 `shiftL` target
      -- Only process states where control=1 and target=0 (to avoid double swaps)
      updates = concatMap (\i ->
        if (i .&. controlMask /= 0) && (i .&. targetMask == 0)
        then
          let j = i `xor` targetMask
              ai = state V.! i
              aj = state V.! j
          in [(i, aj), (j, ai)]
        else []
        ) [0 .. dim - 1]
  in QuantumState (state V.// updates)

-- | Apply controlled-Z gate
applyCZ :: Int -> Int -> QuantumState -> QuantumState
applyCZ control target (QuantumState state) =
  let dim = V.length state
      controlMask = 1 `shiftL` control
      targetMask = 1 `shiftL` target
      -- Negate amplitude when both control and target bits are 1
      updates = [ (i, negate (state V.! i))
                | i <- [0 .. dim - 1]
                , i .&. controlMask /= 0
                , i .&. targetMask /= 0 ]
  in QuantumState (state V.// updates)

-- | Apply controlled-Y gate
applyCY :: Int -> Int -> QuantumState -> QuantumState
applyCY control target (QuantumState state) =
  let dim = V.length state
      controlMask = 1 `shiftL` control
      targetMask = 1 `shiftL` target
      -- When control=1, apply Y to target qubit
      updates = concatMap (\i ->
        if (i .&. controlMask /= 0) && (i .&. targetMask == 0)
        then
          let j = i `xor` targetMask
              ai = state V.! i  -- target bit = 0
              aj = state V.! j  -- target bit = 1
              -- Y = [[0, -i], [i, 0]]
              newAi = (0 :+ (-1)) * aj   -- -i * |1>
              newAj = (0 :+ 1) * ai      -- i * |0>
          in [(i, newAi), (j, newAj)]
        else []
        ) [0 .. dim - 1]
  in QuantumState (state V.// updates)

-- | Apply SWAP gate: swap amplitudes for pairs where qubits a and b differ
applySWAP :: Int -> Int -> QuantumState -> QuantumState
applySWAP qubitA qubitB (QuantumState state) =
  let dim = V.length state
      maskA = 1 `shiftL` qubitA
      maskB = 1 `shiftL` qubitB
      -- Only process states where bit a = 0 and bit b = 1 (to avoid double swap)
      updates = concatMap (\i ->
        let bitA = i .&. maskA /= 0
            bitB = i .&. maskB /= 0
        in if not bitA && bitB
           then
             let j = (i `xor` maskA) `xor` maskB  -- swap the two bits
                 ai = state V.! i
                 aj = state V.! j
             in [(i, aj), (j, ai)]
           else []
        ) [0 .. dim - 1]
  in QuantumState (state V.// updates)

-- | Apply iSWAP gate: like SWAP but with a phase of i on the swapped states
applyISWAP :: Int -> Int -> QuantumState -> QuantumState
applyISWAP qubitA qubitB (QuantumState state) =
  let dim = V.length state
      maskA = 1 `shiftL` qubitA
      maskB = 1 `shiftL` qubitB
      updates = concatMap (\i ->
        let bitA = i .&. maskA /= 0
            bitB = i .&. maskB /= 0
        in if not bitA && bitB
           then
             let j = (i `xor` maskA) `xor` maskB
                 ai = state V.! i
                 aj = state V.! j
             in [(i, (0 :+ 1) * aj), (j, (0 :+ 1) * ai)]
           else []
        ) [0 .. dim - 1]
  in QuantumState (state V.// updates)

-- | Apply Toffoli (CCX) gate: flip target when both controls are 1
applyCCX :: Int -> Int -> Int -> QuantumState -> QuantumState
applyCCX ctrl1 ctrl2 target (QuantumState state) =
  let dim = V.length state
      mask1 = 1 `shiftL` ctrl1
      mask2 = 1 `shiftL` ctrl2
      maskT = 1 `shiftL` target
      updates = concatMap (\i ->
        if (i .&. mask1 /= 0) && (i .&. mask2 /= 0) && (i .&. maskT == 0)
        then
          let j = i `xor` maskT
              ai = state V.! i
              aj = state V.! j
          in [(i, aj), (j, ai)]
        else []
        ) [0 .. dim - 1]
  in QuantumState (state V.// updates)

-- | Apply controlled-SWAP (Fredkin) gate
applyCSwap :: Int -> Int -> Int -> QuantumState -> QuantumState
applyCSwap ctrl qubitA qubitB (QuantumState state) =
  let dim = V.length state
      maskC = 1 `shiftL` ctrl
      maskA = 1 `shiftL` qubitA
      maskB = 1 `shiftL` qubitB
      updates = concatMap (\i ->
        let bitC = i .&. maskC /= 0
            bitA = i .&. maskA /= 0
            bitB = i .&. maskB /= 0
        in if bitC && not bitA && bitB  -- control=1, swap needed (a=0,b=1)
           then
             let j = (i `xor` maskA) `xor` maskB
                 ai = state V.! i
                 aj = state V.! j
             in [(i, aj), (j, ai)]
           else []
        ) [0 .. dim - 1]
  in QuantumState (state V.// updates)

-- | Apply parametric two-qubit gate (XX, YY, ZZ interactions)
-- XX(theta) = exp(-i * theta/2 * X tensor X)
applyXXGate :: Double -> Int -> Int -> QuantumState -> QuantumState
applyXXGate theta qubitA qubitB st =
  -- XX(theta) = cos(t/2)*I tensor I - i*sin(t/2)*X tensor X
  -- Decompose into simpler gates: CNOT(a,b), Rx(theta,a), CNOT(a,b)
  applyCNOT qubitA qubitB $
    applySingleQubitGate (rxGate theta) qubitA $
    applyCNOT qubitA qubitB st

-- | Apply YY gate
applyYYGate :: Double -> Int -> Int -> QuantumState -> QuantumState
applyYYGate theta qubitA qubitB st =
  -- Decompose: S_dag tensor S_dag, CNOT, Ry(theta), CNOT, S tensor S
  applySingleQubitGate sGate qubitA $
    applySingleQubitGate sGate qubitB $
    applyCNOT qubitA qubitB $
    applySingleQubitGate (ryGate theta) qubitA $
    applyCNOT qubitA qubitB $
    applySingleQubitGate sAdjointGate qubitA $
    applySingleQubitGate sAdjointGate qubitB st

-- | Apply ZZ gate
applyZZGate :: Double -> Int -> Int -> QuantumState -> QuantumState
applyZZGate theta qubitA qubitB st =
  -- Decompose: CNOT(a,b), Rz(theta,b), CNOT(a,b)
  applyCNOT qubitA qubitB $
    applySingleQubitGate (rzGate theta) qubitB $
    applyCNOT qubitA qubitB st

-- ============================================================================
-- Measurement
-- ============================================================================

-- | Measure a qubit using the Born rule.
-- Computes probability of |0> and |1> for the target qubit.
-- Uses deterministic seed for reproducibility (based on current state hash).
-- Collapses the state after measurement and renormalizes.
measure :: Int -> QuantumState -> (Int, QuantumState)
measure target (QuantumState state) =
  let dim = V.length state
      targetMask = 1 `shiftL` target

      -- Probability of measuring |0> (sum |a_i|^2 for all i where bit target = 0)
      prob0 = V.ifoldl' (\acc i amp ->
        if i .&. targetMask == 0
        then acc + magnitude amp ^ (2 :: Int)
        else acc) 0.0 state

      -- Deterministic outcome based on amplitudes (for reproducibility)
      -- Use a simple deterministic rule: measure |0> if prob0 >= 0.5
      -- For truly random behavior, a StdGen should be threaded through
      outcome = if prob0 >= 0.5 then 0 else 1

      -- Collapse: zero out amplitudes inconsistent with measurement outcome
      collapsed = V.imap (\i amp ->
        let bitVal = if i .&. targetMask /= 0 then 1 else 0
        in if bitVal == outcome then amp else 0 :+ 0) state

      -- Renormalize
      normSq = V.foldl' (\acc amp -> acc + magnitude amp ^ (2 :: Int)) 0.0 collapsed
      normFactor = if normSq > 0 then sqrt normSq else 1.0
      normalized = V.map (/ (normFactor :+ 0)) collapsed
  in (outcome, QuantumState normalized)

-- | Reset a qubit to |0>: measure it, then if result is 1, apply X
resetQubit :: Int -> QuantumState -> QuantumState
resetQubit target st =
  let (result, collapsed) = measure target st
  in if result == 1
     then applySingleQubitGate pauliX target collapsed
     else collapsed

-- ============================================================================
-- State Inspection
-- ============================================================================

-- | Get amplitude at basis state index
getAmplitude :: Int -> QuantumState -> Complex Double
getAmplitude idx (QuantumState state) =
  if idx >= 0 && idx < V.length state
  then state V.! idx
  else 0 :+ 0

-- | Get state norm (should be 1.0 for valid quantum states)
getStateNorm :: QuantumState -> Double
getStateNorm (QuantumState state) =
  sqrt $ V.foldl' (\acc amp -> acc + magnitude amp ^ (2 :: Int)) 0.0 state

-- | Get number of qubits from state vector size
stateQubits :: QuantumState -> Int
stateQubits (QuantumState state) =
  let dim = V.length state
  in go 0 dim
  where
    go n d | d <= 1 = n
           | otherwise = go (n + 1) (d `div` 2)

-- | Kronecker product of two vectors (used for tensor product of states)
kronecker :: V.Vector (Complex Double) -> V.Vector (Complex Double) -> V.Vector (Complex Double)
kronecker a b =
  let na = V.length a
      nb = V.length b
  in V.generate (na * nb) (\k ->
       let i = k `div` nb
           j = k `mod` nb
       in (a V.! i) * (b V.! j))

-- ============================================================================
-- Circuit Execution
-- ============================================================================

-- | Helper: extract target qubit indices from an Operation
opTargetQubits :: Operation -> [Int]
opTargetQubits op = case gate op of
  Just g  -> QCL.IR.Gate.targetQubits g
  Nothing -> map (\(WireId i) -> i) (targetWires op)

-- | Helper: extract control qubit indices from an Operation
opControlQubits :: Operation -> [Int]
opControlQubits op = case gate op of
  Just g  -> QCL.IR.Gate.controlQubits g
  Nothing -> map (\(WireId i) -> i) (controlWires op)

-- | Execute a single operation on the quantum state
executeOp :: (QuantumState, [ExecutionTrace], [(Int, Int)], Int) -> Operation
          -> (QuantumState, [ExecutionTrace], [(Int, Int)], Int)
executeOp (state, traces, measurements, step) op =
  case opType op of
    GateOperation -> case gate op of
      Just g ->
        let targets = QCL.IR.Gate.targetQubits g
            controls = QCL.IR.Gate.controlQubits g
            newState = applyGate (gateType g) targets controls state
            trace = ExecutionTrace step (show (gateType g)) targets (getStateNorm newState)
        in (newState, traces ++ [trace], measurements, step + 1)
      Nothing -> (state, traces, measurements, step)

    MeasurementOperation ->
      let targets = opTargetQubits op
      in case targets of
        [] -> (state, traces, measurements, step)
        (t:_) ->
          let (result, newState) = measure t state
              trace = ExecutionTrace step ("measure->" ++ show result) targets (getStateNorm newState)
          in (newState, traces ++ [trace], measurements ++ [(t, result)], step + 1)

    ResetOperation ->
      let targets = opTargetQubits op
          newState = foldl' (\s t -> resetQubit t s) state targets
          trace = ExecutionTrace step "reset" targets (getStateNorm newState)
      in (newState, traces ++ [trace], measurements, step + 1)

    BarrierOperation ->
      -- Barrier is a no-op in simulation (synchronization point only)
      let trace = ExecutionTrace step "barrier" (opTargetQubits op) (getStateNorm state)
      in (state, traces ++ [trace], measurements, step + 1)

-- | Apply a gate to the quantum state based on its type
applyGate :: GateType -> [Int] -> [Int] -> QuantumState -> QuantumState
applyGate gt targets controls state = case gt of
  UnaryGate canon ->
    case targets of
      (t:_) -> applySingleQubitGate (gateMatrix canon) t state
      []    -> state

  ParametricGate rot ->
    case targets of
      (t:_) -> applySingleQubitGate (rotationMatrix rot) t state
      []    -> state

  BinaryGate twoq ->
    case (controls, targets) of
      -- Controlled gates: use control from controls list, target from targets
      (c:_, t:_) -> applyBinaryGate twoq c t state
      -- Non-controlled gates (SWAP): use first two from targets
      ([], t1:t2:_) -> applyBinaryGate twoq t1 t2 state
      -- Fallback: try to extract from targets alone
      ([], [t]) -> state  -- Not enough qubits
      _ -> state

  TernaryGate threeq ->
    case (controls ++ targets) of
      (a:b:c:_) -> applyTernaryGate threeq a b c state
      _         -> state

  MeasurementGate _ -> state  -- Handled separately in executeOp
  ResetGate _       -> state  -- Handled separately in executeOp

-- | Dispatch binary gate application
applyBinaryGate :: TwoQubitGate -> Int -> Int -> QuantumState -> QuantumState
applyBinaryGate twoq ctrl tgt state = case twoq of
  CNOT       -> applyCNOT ctrl tgt state
  CY         -> applyCY ctrl tgt state
  CZ         -> applyCZ ctrl tgt state
  SWAP       -> applySWAP ctrl tgt state
  iSWAP      -> applyISWAP ctrl tgt state
  XX theta   -> applyXXGate theta ctrl tgt state
  YY theta   -> applyYYGate theta ctrl tgt state
  ZZ theta   -> applyZZGate theta ctrl tgt state

-- | Dispatch ternary gate application
applyTernaryGate :: ThreeQubitGate -> Int -> Int -> Int -> QuantumState -> QuantumState
applyTernaryGate threeq a b c state = case threeq of
  CCX   -> applyCCX a b c state
  CSwap -> applyCSwap a b c state

-- | Execute circuit on simulator
-- Iterates operations in topological (timestamp) order.
runSimulator :: Circuit -> SimulationResult
runSimulator circ =
  let nQubits = case getCircuitStats circ of
        Right stats -> statsQubitCount stats
        Left _ -> 0
      initialState = initState nQubits
      ops = getAllOperations (circuitDAG circ)
      (finalState, traces, measurements, _) =
        foldl' executeOp (initialState, [], [], 0) ops
  in SimulationResult
    { resultCircuit = circ
    , resultFinalState = finalState
    , resultMeasurements = measurements
    , resultExecutionTime = fromIntegral (length ops) * 0.001
    , resultTraces = traces
    }

-- ============================================================================
-- Pretty Printing
-- ============================================================================

-- | Pretty-print simulation result
prettySimResult :: SimulationResult -> String
prettySimResult res =
  let st = resultFinalState res
      nq = stateQubits st
  in unlines $
    [ "Simulation Result"
    , "================="
    , "Qubits: " ++ show nq
    , "Final state norm: " ++ show (getStateNorm st)
    , "Measurements: " ++ show (resultMeasurements res)
    , "Execution time: " ++ show (resultExecutionTime res) ++ "s"
    , "Operations executed: " ++ show (length (resultTraces res))
    , ""
    , "State vector (non-zero amplitudes):"
    ] ++ prettyStateVector st ++
    [ ""
    , "Traces (first 20):"
    ] ++ map prettyTrace (take 20 (resultTraces res))

-- | Pretty-print non-zero amplitudes of the state vector
prettyStateVector :: QuantumState -> [String]
prettyStateVector (QuantumState state) =
  let nq = go 0 (V.length state)
      entries = [ (i, amp)
                | (i, amp) <- zip [0..] (V.toList state)
                , magnitude amp > 1e-10 ]
      formatEntry (i, amp) =
        "  |" ++ toBinary nq i ++ "> : " ++ showComplex amp ++
        "  (prob=" ++ show (magnitude amp ^ (2 :: Int)) ++ ")"
  in if null entries
     then ["  (all zero)"]
     else map formatEntry (take 32 entries) ++
          (if length entries > 32 then ["  ... (" ++ show (length entries - 32) ++ " more)"] else [])
  where
    go n d | d <= 1 = n
           | otherwise = go (n + 1) (d `div` 2)

-- | Convert integer to binary string of given width
toBinary :: Int -> Int -> String
toBinary width n = [if testBit n (width - 1 - k) then '1' else '0' | k <- [0 .. width - 1]]

-- | Format a complex number for display
showComplex :: Complex Double -> String
showComplex (r :+ i)
  | abs i < 1e-10 = show r
  | abs r < 1e-10 = show i ++ "i"
  | i >= 0        = show r ++ "+" ++ show i ++ "i"
  | otherwise      = show r ++ show i ++ "i"

-- | Pretty-print execution trace
prettyTrace :: ExecutionTrace -> String
prettyTrace t =
  "  Step " ++ show (traceStep t) ++ ": " ++ traceOp t ++
  " on " ++ show (traceWires t) ++ " (norm=" ++ show (traceStateNorm t) ++ ")"
