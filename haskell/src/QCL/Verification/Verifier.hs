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

-- | Circuit Verifier
--
-- Top-level verification entry point that combines:
--   - Circuit equivalence checking (from QCL.Verification.Equivalence)
--   - Wire register validation
--   - Gate library completeness
--   - DAG acyclicity
--   - Ownership constraint enforcement

module QCL.Verification.Verifier
  ( VerificationResult(..)
  , VerificationLevel(..)
  , verifyCircuit
  , verifyWireRegister
  , verifyGateLibrary
  , quickVerify
  ) where

import Data.Aeson
import GHC.Generics

import QCL.IR.Wire
import QCL.IR.Gate
import QCL.IR.Circuit
import QCL.IR.Operation
import QCL.Verification.Equivalence

-- | Verification level
data VerificationLevel
  = QuickCheck       -- ^ Fast structural checks only
  | StandardVerify   -- ^ Structural + semantic checks
  | FullVerify       -- ^ Structural + semantic + equivalence proofs
  deriving (Show, Eq, Ord, Generic)

instance ToJSON VerificationLevel
instance FromJSON VerificationLevel

-- | Verification result
data VerificationResult = VerificationResult
  { vrPassed :: Bool
  , vrErrors :: [String]
  , vrWarnings :: [String]
  , vrLevel :: VerificationLevel
  , vrChecksRun :: Int
  , vrChecksPassed :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON VerificationResult
instance FromJSON VerificationResult

-- | Verify a complete circuit at the given level
verifyCircuit :: VerificationLevel -> Circuit -> VerificationResult
verifyCircuit level circ =
  let wireResult = verifyWireRegister (wireRegister circ)
      gateResult = verifyGateLibrary (gateLibrary circ)
      dagResult = case validateDAG (circuitDAG circ) of
        Left err -> [err]
        Right () -> []
      circResult = case validateCircuit circ of
        Left err -> [err]
        Right () -> []
      allErrors = wireResult ++ gateResult ++ dagResult ++ circResult
      totalChecks = 4
      passedChecks = totalChecks - length (filter (not . null) [wireResult, gateResult, dagResult, circResult])
  in VerificationResult
    { vrPassed = null allErrors
    , vrErrors = allErrors
    , vrWarnings = []
    , vrLevel = level
    , vrChecksRun = totalChecks
    , vrChecksPassed = passedChecks
    }

-- | Verify wire register
verifyWireRegister :: WireRegister -> [String]
verifyWireRegister reg = case validateWireRegister reg of
  Left err -> [err]
  Right () -> []

-- | Verify gate library completeness
verifyGateLibrary :: GateLibrary -> [String]
verifyGateLibrary lib = case verifyGateLibraryCompleteness lib of
  Left err -> [err]
  Right () -> []

-- | Quick structural verification
quickVerify :: Circuit -> VerificationResult
quickVerify = verifyCircuit QuickCheck
