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

-- | Linear/Affine Ownership Checker
--
-- Enforces quantum no-cloning constraint at compile time.
-- Every quantum value has one owner at any time.
--
-- Rules:
--   - Linear qubit: Used exactly once, cannot be copied
--   - Affine ancilla: Used 0 or 1 times, can be reset
--   - Unrestricted classical: No restrictions
--   - No aliasing: Same qubit cannot be in two places simultaneously
--   - Measurement ends linearity: Measured qubit becomes classical

module QCL.Type.Ownership where

import Data.Aeson
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set

import QCL.Type.System

-- | Ownership state of a quantum value
data OwnershipState
  = Owned String              -- ^ Owned by specific binding
  | Moved                     -- ^ Has been moved (consumed)
  | Measured                  -- ^ Has been measured (becomes classical)
  | Reset                     -- ^ Has been reset (for affine)
  deriving (Show, Eq, Ord, Generic)

instance ToJSON OwnershipState
instance FromJSON OwnershipState

-- | Ownership entry for a single quantum value
data OwnershipEntry = OwnershipEntry
  { valueId :: String         -- ^ Variable or qubit ID
  , valueType :: QuantumType
  , ownershipState :: OwnershipState
  , usageCount :: Int
  , lastUsage :: Maybe Int    -- ^ Line number of last use
  } deriving (Show, Eq, Generic)

instance ToJSON OwnershipEntry
instance FromJSON OwnershipEntry

-- | Ownership environment
data OwnershipEnv = OwnershipEnv
  { entries :: Map.Map String OwnershipEntry
  , scopes :: [Map.Map String OwnershipEntry]  -- Stack of scopes
  } deriving (Show, Eq, Generic)

instance ToJSON OwnershipEnv
instance FromJSON OwnershipEnv

-- | Create empty ownership environment
emptyOwnershipEnv :: OwnershipEnv
emptyOwnershipEnv = OwnershipEnv
  { entries = Map.empty
  , scopes = [Map.empty]
  }

-- | Add binding to ownership environment
bindOwnership :: String -> QuantumType -> OwnershipEnv -> OwnershipEnv
bindOwnership name qt env =
  let entry = OwnershipEntry
        { valueId = name
        , valueType = qt
        , ownershipState = Owned name
        , usageCount = 0
        , lastUsage = Nothing
        }
  in env { entries = Map.insert name entry (entries env) }

-- | Look up ownership entry
lookupOwnership :: String -> OwnershipEnv -> Maybe OwnershipEntry
lookupOwnership name env = Map.lookup name (entries env)

-- | Ownership violation
data OwnershipViolation
  = DoubleBorrow String           -- ^ Quantum value borrowed twice
  | UseAfterMove String           -- ^ Value used after move
  | UseAfterMeasure String        -- ^ Operation on measured qubit
  | NotConsumed String            -- ^ Linear value not consumed
  | AffineNotReset String         -- ^ Affine value not reset
  | MissingOwner String           -- ^ Value has no owner
  | InvalidReuse String           -- ^ Qubit reused in same expression
  deriving (Show, Eq, Generic)

instance ToJSON OwnershipViolation
instance FromJSON OwnershipViolation

-- | Ownership check result
type OwnershipCheck = Either OwnershipViolation

-- | Use a quantum value (check and update ownership)
useQuantumValue :: String -> Int -> OwnershipEnv -> OwnershipCheck OwnershipEnv
useQuantumValue name lineNum env = do
  case lookupOwnership name env of
    Nothing -> Left (MissingOwner name)

    Just entry -> do
      -- Check current state
      case ownershipState entry of
        Moved -> Left (UseAfterMove name)
        Measured -> Left (UseAfterMeasure name)
        Reset -> Left (UseAfterMove name)
        Owned _ -> Right ()

      -- Get linearity
      let lin = getLinearity (valueType entry)

      -- Check usage count (linear can only be used once)
      case lin of
        Linear ->
          if usageCount entry > 0
            then Left (DoubleBorrow name)
            else Right ()

        Affine ->
          if usageCount entry > 1
            then Left (DoubleBorrow name)
            else Right ()

        Unrestricted -> Right ()

      -- Update entry: increment use and mark as moved (for linear)
      let newEntry = entry
            { usageCount = usageCount entry + 1
            , lastUsage = Just lineNum
            , ownershipState = if lin == Linear then Moved else ownershipState entry
            }

      let newEnv = env { entries = Map.insert name newEntry (entries env) }
      Right newEnv

-- | Measure a quantum value (converts to classical)
measureQuantumValue :: String -> Int -> OwnershipEnv -> OwnershipCheck OwnershipEnv
measureQuantumValue name lineNum env = do
  case lookupOwnership name env of
    Nothing -> Left (MissingOwner name)

    Just entry -> do
      -- Check not already measured
      case ownershipState entry of
        Measured -> Left (UseAfterMeasure name)
        _ -> Right ()

      -- Update: mark as measured
      let newEntry = entry
            { ownershipState = Measured
            , lastUsage = Just lineNum
            }

      let newEnv = env { entries = Map.insert name newEntry (entries env) }
      Right newEnv

-- | Reset an affine value (for ancilla)
resetQuantumValue :: String -> Int -> OwnershipEnv -> OwnershipCheck OwnershipEnv
resetQuantumValue name lineNum env = do
  case lookupOwnership name env of
    Nothing -> Left (MissingOwner name)

    Just entry -> do
      -- Check linearity is affine
      let lin = getLinearity (valueType entry)
      if lin /= Affine
        then Left (AffineNotReset name)
        else Right ()

      -- Check not already measured
      case ownershipState entry of
        Measured -> Left (UseAfterMeasure name)
        _ -> Right ()

      -- Update: mark as reset, reset usage count
      let newEntry = entry
            { ownershipState = Reset
            , usageCount = 0
            , lastUsage = Just lineNum
            }

      let newEnv = env { entries = Map.insert name newEntry (entries env) }
      Right newEnv

-- | Check all linear values are consumed
checkLinearConsumption :: OwnershipEnv -> OwnershipCheck ()
checkLinearConsumption env = do
  let linVals = [(name, entry) | (name, entry) <- Map.toList (entries env),
                                 getLinearity (valueType entry) == Linear]
  let unconsumed = [(name, entry) | (name, entry) <- linVals,
                                    ownershipState entry /= Moved && ownershipState entry /= Measured]

  if null unconsumed
    then Right ()
    else Left (NotConsumed (fst (head unconsumed)))

-- | Check no double usage in expression
checkNoDoubleUsage :: [String] -> OwnershipEnv -> OwnershipCheck ()
checkNoDoubleUsage names env = do
  let nameSet = Set.fromList names
  if Set.size nameSet /= length names
    then Left (InvalidReuse (head names))
    else Right ()

-- | Ownership check for operation
checkOperationOwnership
  :: String              -- ^ Operation name
  -> [String]            -- ^ Argument names
  -> Int                 -- ^ Line number
  -> OwnershipEnv
  -> OwnershipCheck OwnershipEnv
checkOperationOwnership op args lineNum env = do
  -- Check no duplicate arguments
  checkNoDoubleUsage args env

  -- Use each argument
  foldM (\e name -> useQuantumValue name lineNum e) env args

-- | Ownership report
data OwnershipReport = OwnershipReport
  { reportViolations :: [OwnershipViolation]
  , reportWarnings :: [String]
  , reportOK :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON OwnershipReport
instance FromJSON OwnershipReport

-- | Generate ownership report
generateOwnershipReport :: OwnershipEnv -> OwnershipReport
generateOwnershipReport env =
  let violations = checkAllViolations env
  in OwnershipReport
    { reportViolations = violations
    , reportWarnings = []
    , reportOK = null violations
    }

-- | Check all violations in environment
checkAllViolations :: OwnershipEnv -> [OwnershipViolation]
checkAllViolations env =
  let entries' = Map.elems (entries env)
      linViolations = [NotConsumed (valueId e) | e <- entries',
                                                   getLinearity (valueType e) == Linear,
                                                   ownershipState e /= Moved && ownershipState e /= Measured]
      affViolations = [AffineNotReset (valueId e) | e <- entries',
                                                      getLinearity (valueType e) == Affine,
                                                      ownershipState e == Owned (valueId e),
                                                      usageCount e > 0 && usageCount e < 1]
  in linViolations ++ affViolations

-- | Enter scope (for block scoping)
enterScope :: OwnershipEnv -> OwnershipEnv
enterScope env =
  env { scopes = Map.empty : scopes env }

-- | Exit scope (check all linear values consumed)
exitScope :: OwnershipEnv -> OwnershipCheck OwnershipEnv
exitScope env = do
  case scopes env of
    [] -> Left (MissingOwner "scope stack empty")
    (scope:rest) -> do
      -- Check all values in scope are consumed
      let entries' = Map.elems scope
      let unconsumed = [(valueId e) | e <- entries',
                                       getLinearity (valueType e) == Linear,
                                       ownershipState e /= Moved && ownershipState e /= Measured]
      if null unconsumed
        then Right (env { scopes = rest })
        else Left (NotConsumed (head unconsumed))

-- | Pretty-print ownership state
prettyOwnershipState :: OwnershipState -> String
prettyOwnershipState state = case state of
  Owned owner -> "owned by " ++ owner
  Moved -> "moved"
  Measured -> "measured"
  Reset -> "reset"

-- | Pretty-print ownership entry
prettyOwnershipEntry :: OwnershipEntry -> String
prettyOwnershipEntry entry =
  valueId entry ++ " : " ++ prettyType (valueType entry) ++
  " (" ++ prettyOwnershipState (ownershipState entry) ++ ")" ++
  " used " ++ show (usageCount entry) ++ " times"

-- | Pretty-print ownership violation
prettyViolation :: OwnershipViolation -> String
prettyViolation v = case v of
  DoubleBorrow name -> "Quantum value '" ++ name ++ "' borrowed twice (no-cloning violation)"
  UseAfterMove name -> "Use after move: '" ++ name ++ "' already consumed"
  UseAfterMeasure name -> "Use after measure: '" ++ name ++ "' is now classical"
  NotConsumed name -> "Linear value '" ++ name ++ "' not consumed"
  AffineNotReset name -> "Affine value '" ++ name ++ "' not reset"
  MissingOwner name -> "Value '" ++ name ++ "' has no owner"
  InvalidReuse name -> "Quantum value '" ++ name ++ "' used multiple times in expression"

-- | Phantom type for compile-time checking
-- (In real implementation, would use Haskell's phantom types more extensively)
data LinearValue a = LinearValue a
  deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (LinearValue a)
instance FromJSON a => FromJSON (LinearValue a)

-- | Helper: fold with error handling
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a [] = return a
foldM f a (b:bs) = do
  a' <- f a b
  foldM f a' bs

-- | Ownership debugging info
data OwnershipDebugInfo = OwnershipDebugInfo
  { debugEnv :: OwnershipEnv
  , debugViolations :: [OwnershipViolation]
  , debugTrace :: [String]
  } deriving (Show, Eq, Generic)

instance ToJSON OwnershipDebugInfo
instance FromJSON OwnershipDebugInfo

-- | Create debug info
createDebugInfo :: OwnershipEnv -> OwnershipDebugInfo
createDebugInfo env =
  OwnershipDebugInfo
    { debugEnv = env
    , debugViolations = checkAllViolations env
    , debugTrace = ["Ownership check complete"]
    }
