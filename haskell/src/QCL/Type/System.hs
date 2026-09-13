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

-- | Quantum Type System
--
-- Type-safe representation of quantum data with linear/affine properties.
-- Prevents invalid quantum operations at compile time.
--
-- Types:
--   - Qubit: Single quantum bit (linear - cannot be duplicated)
--   - QRegister[n]: n-qubit register (linear)
--   - Ancilla: Auxiliary qubit with reset semantics
--   - ClassicalBit: Classical 0 or 1 (unrestricted)
--   - QuantumKey: Cryptographic key material (linear + no-copy)
--   - QuantumNonce: Nonce value (linear + consumable)
--   - Measured: Result of measurement (classic, unrestricted)

module QCL.Type.System where

import Data.Aeson
import Data.List (intercalate)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Base quantum types
data QuantumType
  = TQubit                          -- ^ Single quantum bit (linear)
  | TQRegister Int                  -- ^ n-qubit register (linear)
  | TAncilla Int                    -- ^ n ancilla qubits (linear, reset-able)
  | TClassicalBit                   -- ^ Classical bit (unrestricted)
  | TClassicalInt                   -- ^ Classical integer (unrestricted)
  | TClassicalFloat                 -- ^ Classical floating-point (unrestricted)
  | TClassicalString                -- ^ Classical string (unrestricted)
  | TMeasured QuantumType           -- ^ Result of measurement (becomes classical)
  | TQuantumKey Int                 -- ^ Cryptographic key (linear, n bits)
  | TQuantumNonce Int               -- ^ Cryptographic nonce (linear, n bits)
  | TQuantumState QuantumType       -- ^ Generic quantum state
  | TVoid                           -- ^ No type (used for operations)
  deriving (Show, Eq, Ord, Generic)

instance ToJSON QuantumType
instance FromJSON QuantumType

-- | Type linearity (ownership) constraint
data Linearity
  = Linear                          -- ^ Must be used exactly once (quantum data)
  | Affine                          -- ^ May be used 0 or 1 times (with reset)
  | Unrestricted                    -- ^ May be used any number of times (classical)
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Linearity
instance FromJSON Linearity

-- | Get linearity of a type
getLinearity :: QuantumType -> Linearity
getLinearity qt = case qt of
  TQubit -> Linear
  TQRegister _ -> Linear
  TAncilla _ -> Affine              -- Ancilla can be reset
  TClassicalBit -> Unrestricted
  TClassicalInt -> Unrestricted
  TMeasured _ -> Unrestricted       -- Measurement result is classical
  TQuantumKey _ -> Linear           -- Keys cannot be copied
  TQuantumNonce _ -> Linear         -- Nonces cannot be reused
  TQuantumState _ -> Linear
  TVoid -> Unrestricted

-- | Type variable binding
data TypeVar = TypeVar String
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TypeVar
instance FromJSON TypeVar

-- | Type binding (for parametric types)
data TypeBinding = TypeBinding
  { bindVar :: TypeVar
  , bindType :: QuantumType
  } deriving (Show, Eq, Generic)

instance ToJSON TypeBinding
instance FromJSON TypeBinding

-- | Type constraint (for verification)
data TypeConstraint
  = MustBeLinear QuantumType
  | MustBeAffine QuantumType
  | MustNotMeasured QuantumType
  | MustBeMeasured QuantumType
  | SameType QuantumType QuantumType
  | RegisterSize Int
  deriving (Show, Eq, Generic)

instance ToJSON TypeConstraint
instance FromJSON TypeConstraint

-- | Type scheme (generic type with constraints)
data TypeScheme = TypeScheme
  { schemeVars :: [TypeVar]
  , schemeType :: QuantumType
  , schemeConstraints :: [TypeConstraint]
  } deriving (Show, Eq, Generic)

instance ToJSON TypeScheme
instance FromJSON TypeScheme

-- | Type context (variable type bindings)
data TypeContext = TypeContext
  { bindings :: Map.Map String QuantumType
  , constraints :: [TypeConstraint]
  } deriving (Show, Eq, Generic)

instance ToJSON TypeContext
instance FromJSON TypeContext

-- | Create empty type context
emptyTypeContext :: TypeContext
emptyTypeContext = TypeContext
  { bindings = Map.empty
  , constraints = []
  }

-- | Add type binding
addBinding :: String -> QuantumType -> TypeContext -> TypeContext
addBinding name qt ctx =
  ctx { bindings = Map.insert name qt (bindings ctx) }

-- | Look up type binding
lookupType :: String -> TypeContext -> Maybe QuantumType
lookupType name ctx = Map.lookup name (bindings ctx)

-- | Add type constraint
addConstraint :: TypeConstraint -> TypeContext -> TypeContext
addConstraint c ctx =
  ctx { constraints = c : constraints ctx }

-- | Check type constraint
checkConstraint :: TypeConstraint -> Either String ()
checkConstraint c = case c of
  MustBeLinear qt ->
    if getLinearity qt == Linear
      then Right ()
      else Left $ "Type must be linear: " ++ show qt

  MustBeAffine qt ->
    let lin = getLinearity qt
    in if lin == Linear || lin == Affine
       then Right ()
       else Left $ "Type must be affine: " ++ show qt

  MustNotMeasured qt -> case qt of
    TMeasured _ -> Left $ "Type must not be measured: " ++ show qt
    _ -> Right ()

  MustBeMeasured qt -> case qt of
    TMeasured _ -> Right ()
    _ -> Left $ "Type must be measured: " ++ show qt

  SameType t1 t2 ->
    if t1 == t2
      then Right ()
      else Left $ "Types must be equal: " ++ show t1 ++ " != " ++ show t2

  RegisterSize n ->
    if n > 0
      then Right ()
      else Left $ "Register size must be positive: " ++ show n

-- | Check all constraints in context
checkAllConstraints :: TypeContext -> Either String ()
checkAllConstraints ctx = do
  mapM_ checkConstraint (constraints ctx)
  Right ()

-- | Type equality
typeEqual :: QuantumType -> QuantumType -> Bool
typeEqual t1 t2 = t1 == t2

-- | Type unification (try to make types equal)
unify :: QuantumType -> QuantumType -> Either String QuantumType
unify t1 t2 =
  if t1 == t2
    then Right t1
    else case (t1, t2) of
      (TVoid, t) -> Right t
      (t, TVoid) -> Right t
      (TMeasured _, TClassicalBit) -> Right TClassicalBit
      (TClassicalBit, TMeasured _) -> Right TClassicalBit
      _ -> Left $ "Cannot unify types: " ++ show t1 ++ " with " ++ show t2

-- | Type substitution (replace type variables)
substitute :: TypeVar -> QuantumType -> QuantumType -> QuantumType
substitute var replacement qt =
  case qt of
    TQRegister n -> TQRegister n
    TAncilla n -> TAncilla n
    TMeasured inner -> TMeasured (substitute var replacement inner)
    TQuantumKey n -> TQuantumKey n
    TQuantumNonce n -> TQuantumNonce n
    TQuantumState inner -> TQuantumState (substitute var replacement inner)
    _ -> qt

-- | Check if type is measurable
isMeasurable :: QuantumType -> Bool
isMeasurable qt = case qt of
  TQubit -> True
  TQRegister _ -> True
  TAncilla _ -> True
  TQuantumState _ -> True
  _ -> False

-- | Check if measurement would produce valid type
measurementResultType :: QuantumType -> Either String QuantumType
measurementResultType qt =
  if isMeasurable qt
    then Right (TMeasured TClassicalBit)
    else Left $ "Cannot measure type: " ++ show qt

-- | Check if type can be reset (ancilla-like)
isResettable :: QuantumType -> Bool
isResettable qt = case qt of
  TAncilla _ -> True
  _ -> getLinearity qt == Affine

-- | Type inference from operation
inferTypeFromOperation :: String -> [QuantumType] -> Either String QuantumType
inferTypeFromOperation op argTypes = case op of
  "H" ->                            -- Hadamard: Qubit → Qubit
    case argTypes of
      [TQubit] -> Right TQubit
      [TQRegister n] -> Right (TQRegister n)
      _ -> Left "Hadamard requires qubit or qubit register"

  "X" ->                            -- Pauli X: Qubit → Qubit
    case argTypes of
      [TQubit] -> Right TQubit
      [TQRegister n] -> Right (TQRegister n)
      _ -> Left "X gate requires qubit or qubit register"

  "CNOT" ->                         -- CNOT: (Qubit, Qubit) → (Qubit, Qubit)
    case argTypes of
      [TQubit, TQubit] -> Right (TQRegister 2)
      [TQRegister n, TQRegister m] | n == m -> Right (TQRegister n)
      _ -> Left "CNOT requires two qubits of same type"

  "measure" ->                      -- Measure: Qubit → ClassicalBit
    case argTypes of
      [TQubit] -> Right TClassicalBit
      [TQRegister n] -> Right (TQRegister n)  -- Each qubit measured separately
      _ -> Left "Measure requires qubit or qubit register"

  "reset" ->                        -- Reset: Qubit → Qubit
    case argTypes of
      [TQubit] -> Right TQubit
      [TAncilla n] -> Right (TAncilla n)
      _ -> Left "Reset requires ancilla or qubit"

  _ -> Left $ "Unknown operation: " ++ op

-- | Pretty-print type
prettyType :: QuantumType -> String
prettyType qt = case qt of
  TQubit -> "qubit"
  TQRegister n -> "qreg[" ++ show n ++ "]"
  TAncilla n -> "ancilla[" ++ show n ++ "]"
  TClassicalBit -> "bit"
  TClassicalInt -> "int"
  TMeasured inner -> prettyType inner ++ " (measured)"
  TQuantumKey n -> "key[" ++ show n ++ "]"
  TQuantumNonce n -> "nonce[" ++ show n ++ "]"
  TQuantumState inner -> "state(" ++ prettyType inner ++ ")"
  TVoid -> "void"

-- | Pretty-print linearity
prettyLinearity :: Linearity -> String
prettyLinearity lin = case lin of
  Linear -> "linear"
  Affine -> "affine"
  Unrestricted -> "unrestricted"

-- | Standard type library
data TypeLibrary = TypeLibrary
  { stdTypes :: Map.Map String QuantumType
  } deriving (Show, Eq, Generic)

instance ToJSON TypeLibrary
instance FromJSON TypeLibrary

-- | Create standard type library
createStandardTypeLibrary :: TypeLibrary
createStandardTypeLibrary = TypeLibrary
  { stdTypes = Map.fromList
      [ ("qubit", TQubit)
      , ("bit", TClassicalBit)
      , ("int", TClassicalInt)
      , ("void", TVoid)
      ]
  }

-- | Get type from library
getStdType :: String -> TypeLibrary -> Maybe QuantumType
getStdType name lib = Map.lookup name (stdTypes lib)

-- | Validate type scheme
validateTypeScheme :: TypeScheme -> Either String ()
validateTypeScheme scheme = do
  mapM_ checkConstraint (schemeConstraints scheme)
  Right ()

-- | Helper: map monadic
mapM_ :: (Monad m) => (a -> m ()) -> [a] -> m ()
mapM_ _ [] = return ()
mapM_ f (x:xs) = f x >> mapM_ f xs

-- | Type error reporting
data TypeError = TypeError
  { errorMessage :: String
  , errorLocation :: Maybe (String, Int)
  , expectedType :: Maybe QuantumType
  , actualType :: Maybe QuantumType
  } deriving (Show, Eq, Generic)

instance ToJSON TypeError
instance FromJSON TypeError

-- | Type checking result
type TypeCheckResult = Either TypeError

-- | Check type compatibility for operation
checkTypeCompatibility :: String -> [QuantumType] -> Either TypeError QuantumType
checkTypeCompatibility op argTypes =
  case inferTypeFromOperation op argTypes of
    Left msg -> Left $ TypeError msg Nothing Nothing Nothing
    Right resType -> Right resType

-- | Common type signatures for gates
gateTypeSignatures :: Map.Map String TypeScheme
gateTypeSignatures = Map.fromList
  [ ("H", TypeScheme [] (TQubit) [MustBeLinear TQubit])
  , ("X", TypeScheme [] (TQubit) [MustBeLinear TQubit])
  , ("Y", TypeScheme [] (TQubit) [MustBeLinear TQubit])
  , ("Z", TypeScheme [] (TQubit) [MustBeLinear TQubit])
  , ("S", TypeScheme [] (TQubit) [MustBeLinear TQubit])
  , ("T", TypeScheme [] (TQubit) [MustBeLinear TQubit])
  , ("measure", TypeScheme [] TClassicalBit [MustBeMeasured (TMeasured TClassicalBit)])
  , ("reset", TypeScheme [] TQubit [MustBeAffine TQubit])
  ]
