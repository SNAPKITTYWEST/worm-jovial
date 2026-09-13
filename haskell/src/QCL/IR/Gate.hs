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

-- | Quantum Gate model with canonical gate vocabulary
--
-- This module defines the complete gate set for quantum circuits,
-- with support for unitary, parameterized, and controlled operations.
--
-- Canonical gates include:
--   - Pauli: X, Y, Z
--   - Hadamard: H
--   - Phase: S, S†, T, T†
--   - Rotations: Rx, Ry, Rz
--   - Two-qubit: CNOT, CY, CZ, SWAP, iSWAP
--   - Three-qubit: Toffoli (CCX)
--
-- Every gate is type-safe and carries its semantic meaning.

module QCL.IR.Gate where

import Data.Aeson
import Data.Complex
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Gate identifier (globally unique within circuit)
newtype GateId = GateId String
  deriving (Show, Eq, Ord, Generic)

instance ToJSON GateId
instance FromJSON GateId

-- | Canonical gate type (unparameterized)
data CanonicalGate
  = I                 -- ^ Identity
  | X                 -- ^ Pauli X (NOT)
  | Y                 -- ^ Pauli Y
  | Z                 -- ^ Pauli Z
  | H                 -- ^ Hadamard
  | S                 -- ^ Phase gate (Z^1/2)
  | S_adjoint         -- ^ S† (Z^-1/2)
  | T                 -- ^ T gate (Z^1/4)
  | T_adjoint         -- ^ T† (Z^-1/4)
  | SqrtX             -- ^ √X
  | SqrtX_adjoint     -- ^ √X†
  | SqrtY             -- ^ √Y
  | SqrtY_adjoint     -- ^ √Y†
  | SqrtZ             -- ^ √Z
  | SqrtZ_adjoint     -- ^ √Z†
  deriving (Show, Eq, Ord, Generic)

instance ToJSON CanonicalGate
instance FromJSON CanonicalGate

-- | Parameterized rotation gate
data RotationGate
  = Rx Double         -- ^ Rotation around X-axis (angle in radians)
  | Ry Double         -- ^ Rotation around Y-axis
  | Rz Double         -- ^ Rotation around Z-axis
  | PhaseShift Double -- ^ Global phase shift
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RotationGate
instance FromJSON RotationGate

-- | Two-qubit gate type
data TwoQubitGate
  = CNOT              -- ^ Controlled NOT (CX)
  | CY                -- ^ Controlled Y
  | CZ                -- ^ Controlled Z
  | SWAP              -- ^ Swap qubits
  | iSWAP             -- ^ iSWAP
  | iSWAP_adjoint     -- ^ iSWAP† (conjugate transpose)
  | XX Double         -- ^ Parametric XX interaction
  | YY Double         -- ^ Parametric YY interaction
  | ZZ Double         -- ^ Parametric ZZ interaction
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TwoQubitGate
instance FromJSON TwoQubitGate

-- | Three-qubit gate type
data ThreeQubitGate
  = CCX               -- ^ Toffoli (controlled-controlled-X)
  | CSwap             -- ^ Controlled SWAP
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ThreeQubitGate
instance FromJSON ThreeQubitGate

-- | Measurement basis
data MeasurementBasis
  = Computational     -- ^ |0⟩, |1⟩ basis (Z basis)
  | Hadamard_basis    -- ^ |+⟩, |-⟩ basis (X basis)
  | Diagonal          -- ^ |↑⟩, |↓⟩ basis (Y basis)
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MeasurementBasis
instance FromJSON MeasurementBasis

-- | Reset operation
data ResetType
  = ResetZero         -- ^ Reset to |0⟩
  | ResetOne          -- ^ Reset to |1⟩
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ResetType
instance FromJSON ResetType

-- | Complete gate type union
data GateType
  = UnaryGate CanonicalGate
  | ParametricGate RotationGate
  | BinaryGate TwoQubitGate
  | TernaryGate ThreeQubitGate
  | MeasurementGate MeasurementBasis
  | ResetGate ResetType
  deriving (Show, Eq, Generic)

instance ToJSON GateType
instance FromJSON GateType

-- | Gate qubit count
gateQubitCount :: GateType -> Int
gateQubitCount gt = case gt of
  UnaryGate _ -> 1
  ParametricGate _ -> 1
  BinaryGate _ -> 2
  TernaryGate _ -> 3
  MeasurementGate _ -> 1
  ResetGate _ -> 1

-- | Adjoint (inverse) of a gate
adjoint :: CanonicalGate -> CanonicalGate
adjoint g = case g of
  I -> I
  X -> X
  Y -> Y
  Z -> Z
  H -> H
  S -> S_adjoint
  S_adjoint -> S
  T -> T_adjoint
  T_adjoint -> T
  SqrtX -> SqrtX_adjoint
  SqrtX_adjoint -> SqrtX
  SqrtY -> SqrtY_adjoint
  SqrtY_adjoint -> SqrtY
  SqrtZ -> SqrtZ_adjoint
  SqrtZ_adjoint -> SqrtZ

-- | Adjoint of rotation (negate angle)
adjointRotation :: RotationGate -> RotationGate
adjointRotation r = case r of
  Rx theta -> Rx (-theta)
  Ry theta -> Ry (-theta)
  Rz theta -> Rz (-theta)
  PhaseShift phi -> PhaseShift (-phi)

-- | Adjoint of two-qubit gate
adjointTwoQubit :: TwoQubitGate -> TwoQubitGate
adjointTwoQubit g = case g of
  CNOT -> CNOT
  CY -> CY
  CZ -> CZ
  SWAP -> SWAP
  iSWAP -> iSWAP_adjoint  -- iSWAP is NOT self-adjoint; adjoint negates the imaginary phase
  iSWAP_adjoint -> iSWAP
  XX theta -> XX (-theta)
  YY theta -> YY (-theta)
  ZZ theta -> ZZ (-theta)

-- | Adjoint of three-qubit gate
adjointThreeQubit :: ThreeQubitGate -> ThreeQubitGate
adjointThreeQubit g = case g of
  CCX -> CCX
  CSwap -> CSwap

-- | Complete gate definition with metadata
data Gate = Gate
  { gateId :: GateId
  , gateType :: GateType
  , gateQubits :: Int
  , timestamp :: Int           -- ^ Time step in circuit
  , controlQubits :: [Int]     -- ^ Control qubit indices (for controlled gates)
  , targetQubits :: [Int]      -- ^ Target qubit indices
  , parameters :: Map.Map String Double  -- ^ Named parameters (angle, etc.)
  , isControlled :: Bool
  , isAdjoint :: Bool
  , decomposition :: Maybe [Gate]  -- ^ Decomposition into simpler gates
  , sourceLocation :: Maybe (String, Int)  -- ^ Source file, line number
  } deriving (Show, Eq, Generic)

instance ToJSON Gate
instance FromJSON Gate

-- | Create a unary gate
createUnaryGate :: GateId -> CanonicalGate -> Int -> Int -> Gate
createUnaryGate gid canon t target =
  Gate
    { gateId = gid
    , gateType = UnaryGate canon
    , gateQubits = 1
    , timestamp = t
    , controlQubits = []
    , targetQubits = [target]
    , parameters = Map.empty
    , isControlled = False
    , isAdjoint = False
    , decomposition = Nothing
    , sourceLocation = Nothing
    }

-- | Create a parametric gate
createParametricGate :: GateId -> RotationGate -> Int -> Int -> Gate
createParametricGate gid rot t target =
  let param = case rot of
        Rx theta -> ("theta", theta)
        Ry theta -> ("theta", theta)
        Rz theta -> ("theta", theta)
        PhaseShift phi -> ("phi", phi)
  in Gate
    { gateId = gid
    , gateType = ParametricGate rot
    , gateQubits = 1
    , timestamp = t
    , controlQubits = []
    , targetQubits = [target]
    , parameters = Map.fromList [param]
    , isControlled = False
    , isAdjoint = False
    , decomposition = Nothing
    , sourceLocation = Nothing
    }

-- | Create a binary (two-qubit) gate
createBinaryGate :: GateId -> TwoQubitGate -> Int -> Int -> Int -> Gate
createBinaryGate gid twoq t control target =
  let isCtrl = case twoq of
        CNOT -> True
        CY -> True
        CZ -> True
        _ -> False
  in Gate
    { gateId = gid
    , gateType = BinaryGate twoq
    , gateQubits = 2
    , timestamp = t
    , controlQubits = if isCtrl then [control] else []
    , targetQubits = [target]
    , parameters = Map.empty
    , isControlled = isCtrl
    , isAdjoint = False
    , decomposition = Nothing
    , sourceLocation = Nothing
    }

-- | Create a measurement gate
createMeasurementGate :: GateId -> MeasurementBasis -> Int -> Int -> Gate
createMeasurementGate gid basis t target =
  Gate
    { gateId = gid
    , gateType = MeasurementGate basis
    , gateQubits = 1
    , timestamp = t
    , controlQubits = []
    , targetQubits = [target]
    , parameters = Map.empty
    , isControlled = False
    , isAdjoint = False
    , decomposition = Nothing
    , sourceLocation = Nothing
    }

-- | Create a reset gate
createResetGate :: GateId -> ResetType -> Int -> Int -> Gate
createResetGate gid resetType t target =
  Gate
    { gateId = gid
    , gateType = ResetGate resetType
    , gateQubits = 1
    , timestamp = t
    , controlQubits = []
    , targetQubits = [target]
    , parameters = Map.empty
    , isControlled = False
    , isAdjoint = False
    , decomposition = Nothing
    , sourceLocation = Nothing
    }

-- | Complete gate library
data GateLibrary = GateLibrary
  { gates :: Map.Map GateId Gate
  , gateCount :: Int
  , canonicalGates :: Set.Set CanonicalGate
  , parameterizedGates :: Set.Set RotationGate
  , twoQubitGates :: Set.Set TwoQubitGate
  , threeQubitGates :: Set.Set ThreeQubitGate
  } deriving (Show, Eq, Generic)

instance ToJSON GateLibrary
instance FromJSON GateLibrary

-- | Create empty gate library
emptyGateLibrary :: GateLibrary
emptyGateLibrary = GateLibrary
  { gates = Map.empty
  , gateCount = 0
  , canonicalGates = Set.empty
  , parameterizedGates = Set.empty
  , twoQubitGates = Set.empty
  , threeQubitGates = Set.empty
  }

-- | Add gate to library
addGate :: Gate -> GateLibrary -> GateLibrary
addGate g lib =
  let updatedGates = Map.insert (gateId g) g (gates lib)
      updatedCanon = case gateType g of
        UnaryGate c -> Set.insert c (canonicalGates lib)
        _ -> canonicalGates lib
      updatedParam = case gateType g of
        ParametricGate r -> Set.insert r (parameterizedGates lib)
        _ -> parameterizedGates lib
      updatedTwo = case gateType g of
        BinaryGate t -> Set.insert t (twoQubitGates lib)
        _ -> twoQubitGates lib
      updatedThree = case gateType g of
        TernaryGate t -> Set.insert t (threeQubitGates lib)
        _ -> threeQubitGates lib
  in lib
    { gates = updatedGates
    , gateCount = gateCount lib + 1
    , canonicalGates = updatedCanon
    , parameterizedGates = updatedParam
    , twoQubitGates = updatedTwo
    , threeQubitGates = updatedThree
    }

-- | Get gate by ID
getGate :: GateId -> GateLibrary -> Maybe Gate
getGate gid lib = Map.lookup gid (gates lib)

-- | Get all gates sorted by timestamp
getAllGates :: GateLibrary -> [Gate]
getAllGates lib = sortBy (comparing timestamp) (Map.elems (gates lib))

-- | Get gates by type
getGatesByType :: GateType -> GateLibrary -> [Gate]
getGatesByType gt lib =
  filter (\g -> gateType g == gt) (Map.elems (gates lib))

-- | Get canonical gates
getCanonicalGates :: GateLibrary -> [Gate]
getCanonicalGates lib =
  filter (\g -> case gateType g of UnaryGate _ -> True; _ -> False) (Map.elems (gates lib))

-- | Get binary gates
getBinaryGates :: GateLibrary -> [Gate]
getBinaryGates lib =
  filter (\g -> case gateType g of BinaryGate _ -> True; _ -> False) (Map.elems (gates lib))

-- | Validate gate
validateGate :: Gate -> Either String ()
validateGate g = do
  -- Check qubit count matches gate type
  let expected = gateQubitCount (gateType g)
  if gateQubits g /= expected
    then Left $ "Gate qubit count mismatch: expected " ++ show expected ++ ", got " ++ show (gateQubits g)
    else Right ()

  -- Check target qubits
  if null (targetQubits g)
    then Left "Gate must have at least one target qubit"
    else Right ()

  -- Check control qubits only for controlled gates
  if isControlled g && null (controlQubits g)
    then Left "Controlled gate must have control qubits"
    else Right ()

  Right ()

-- | Decompose complex gate into simpler gates
-- Some gates can be decomposed into more fundamental operations
decompose :: Gate -> Either String [Gate]
decompose g = do
  case gateType g of
    -- H = (1/√2) * (X + Z), but use Rx+Rz decomposition
    UnaryGate H ->
      let (GateId base) = gateId g
          rz1 = createParametricGate (GateId (base ++ "_rz1")) (Rz pi) (timestamp g) (targetQubits g !! 0)
          rx = createParametricGate (GateId (base ++ "_rx")) (Rx (pi/2)) (timestamp g + 1) (targetQubits g !! 0)
          rz2 = createParametricGate (GateId (base ++ "_rz2")) (Rz pi) (timestamp g + 2) (targetQubits g !! 0)
      in Right [rz1, rx, rz2]

    -- S = T², decompose to Rz
    UnaryGate S ->
      let (GateId base) = gateId g
          rz = createParametricGate (GateId (base ++ "_rz")) (Rz (pi/2)) (timestamp g) (targetQubits g !! 0)
      in Right [rz]

    -- T = Rz(π/4)
    UnaryGate T ->
      let (GateId base) = gateId g
          rz = createParametricGate (GateId (base ++ "_rz")) (Rz (pi/4)) (timestamp g) (targetQubits g !! 0)
      in Right [rz]

    -- CNOT = (I⊗H)(CZ)(I⊗H): H on target, CZ, H on target
    BinaryGate CNOT ->
      let (GateId base) = gateId g
          target = head (targetQubits g)
          control = head (controlQubits g)
          h1 = createUnaryGate (GateId (base ++ "_h1")) H (timestamp g) target
          cz = createBinaryGate (GateId (base ++ "_cz")) CZ (timestamp g + 1) control target
          h2 = createUnaryGate (GateId (base ++ "_h2")) H (timestamp g + 2) target
      in Right [h1, cz, h2]

    -- Single gates with no decomposition
    UnaryGate _ -> Right [g]
    ParametricGate _ -> Right [g]

    -- Multi-qubit gates typically not decomposed here
    BinaryGate _ -> Right [g]
    TernaryGate _ -> Right [g]

    -- Measurement cannot be decomposed
    MeasurementGate _ -> Right [g]

    -- Reset cannot be decomposed
    ResetGate _ -> Right [g]

-- | Check if two gates can commute
-- Two gates commute if they operate on different qubits or are both diagonal
canCommute :: Gate -> Gate -> Bool
canCommute g1 g2 =
  let qubits1 = Set.fromList (controlQubits g1 ++ targetQubits g1)
      qubits2 = Set.fromList (controlQubits g2 ++ targetQubits g2)
  in Set.null (Set.intersection qubits1 qubits2)

-- | Check if gate is unitary
isUnitary :: Gate -> Bool
isUnitary g = case gateType g of
  UnaryGate _ -> True
  ParametricGate _ -> True
  BinaryGate _ -> True
  TernaryGate _ -> True
  MeasurementGate _ -> False  -- Measurement is not unitary
  ResetGate _ -> False         -- Reset is not unitary

-- | Get gate matrix (Pauli matrices)
-- Returns upper-left 2x2 block for unary gates
getPauliMatrix :: CanonicalGate -> [[Complex Double]]
getPauliMatrix g = case g of
  I -> [[1, 0], [0, 1]]
  X -> [[0, 1], [1, 0]]
  Y -> [[0, (-1 :+ 0)], [1 :+ 0, 0]]
  Z -> [[1, 0], [0, -1]]
  H -> let s = 1 / sqrt 2 in [[s, s], [s, -s]]
  S -> [[1, 0], [0, 0 :+ 1]]
  S_adjoint -> [[1, 0], [0, 0 :- 1]]
  T -> [[1, 0], [0, exp (0 :+ (pi/4))]]
  T_adjoint -> [[1, 0], [0, exp (0 :- (pi/4))]]
  SqrtX -> [[0.5 :+ 0.5, 0.5 :- 0.5], [0.5 :- 0.5, 0.5 :+ 0.5]]
  SqrtX_adjoint -> [[0.5 :- 0.5, 0.5 :+ 0.5], [0.5 :+ 0.5, 0.5 :- 0.5]]
  SqrtY -> [[0.5 :+ 0.5, (-0.5) :- 0.5], [0.5 :+ 0.5, 0.5 :+ 0.5]]
  SqrtY_adjoint -> [[0.5 :- 0.5, 0.5 :- 0.5], [(-0.5) :+ 0.5, 0.5 :- 0.5]]
  SqrtZ -> [[exp (0 :+ (pi/4)), 0], [0, exp (0 :- (pi/4))]]
  SqrtZ_adjoint -> [[exp (0 :- (pi/4)), 0], [0, exp (0 :+ (pi/4))]]

-- | Verify canonical gate set is complete
verifyGateLibraryCompleteness :: GateLibrary -> Either String ()
verifyGateLibraryCompleteness lib = do
  -- Check essential gates exist
  let essentialUnary = [I, X, Y, Z, H, S, T]
  let hasEssential = all (\g -> Set.member g (canonicalGates lib)) essentialUnary
  if not hasEssential
    then Left "Gate library missing essential unary gates"
    else Right ()

  -- Check two-qubit gates
  let hasTwo = any (\g -> Set.member g (twoQubitGates lib)) [CNOT, CZ, SWAP]
  if not hasTwo
    then Left "Gate library missing essential two-qubit gates"
    else Right ()

  Right ()

-- | Create a standard quantum gate library with all canonical gates
createStandardGateLibrary :: GateLibrary
createStandardGateLibrary =
  let lib0 = emptyGateLibrary
      -- Add Pauli gates
      libPauli = foldl (\l (c, i) -> addGate (createUnaryGate (GateId ("pauli_" ++ show i)) c 0 0) l)
                       lib0
                       [(I, 0), (X, 1), (Y, 2), (Z, 3)]
      -- Add Hadamard
      libH = addGate (createUnaryGate (GateId "hadamard") H 0 0) libPauli
      -- Add Phase gates
      libPhase = foldl (\l (c, i) -> addGate (createUnaryGate (GateId ("phase_" ++ show i)) c 0 0) l)
                       libH
                       [(S, 0), (S_adjoint, 1), (T, 2), (T_adjoint, 3)]
      -- Add Sqrt gates
      libSqrt = foldl (\l (c, i) -> addGate (createUnaryGate (GateId ("sqrt_" ++ show i)) c 0 0) l)
                      libPhase
                      [(SqrtX, 0), (SqrtX_adjoint, 1), (SqrtY, 2), (SqrtY_adjoint, 3), (SqrtZ, 4), (SqrtZ_adjoint, 5)]
  in libSqrt

-- | Verify standard gate library
verifyStandardGateLibrary :: Either String ()
verifyStandardGateLibrary =
  verifyGateLibraryCompleteness createStandardGateLibrary
