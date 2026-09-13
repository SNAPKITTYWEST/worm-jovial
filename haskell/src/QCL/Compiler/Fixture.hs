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

{-# LANGUAGE OverloadedStrings #-}

-- | 743-Wire Test Fixture
--
-- Provides the canonical 743-wire fixture for validation testing.
-- This fixture represents the reference quantum wire configuration
-- extracted from Quipper source with alternating initial states.

module QCL.Compiler.Fixture
  ( FixtureResult(..)
  , runFixtureValidation
  , run743WireFixture
  , fixtureWireCount
  , fixtureRegister
  ) where

import QCL.IR.Wire
import QCL.IR.Circuit
import QCL.IR.Gate
import qualified Data.Map as Map

-- | Fixture validation result
data FixtureResult = FixtureResult
  { frPassed :: Bool
  , frWireCount :: Int
  , frErrors :: [String]
  , frDetails :: [String]
  } deriving (Show, Eq)

-- | Expected wire count for the canonical fixture
fixtureWireCount :: Int
fixtureWireCount = 743

-- | Get the canonical 743-wire register
fixtureRegister :: WireRegister
fixtureRegister = canonical743Wires

-- | Run the full 743-wire fixture validation
run743WireFixture :: FixtureResult
run743WireFixture =
  let reg = canonical743Wires
      checks =
        [ ("Wire count == 743", registerSize reg == 743)
        , ("Total allocated == 743", totalAllocated reg == 743)
        , ("Total consumed == 0", totalConsumed reg == 0)
        , ("All wires present", length (getAllWires reg) == 743)
        ]
      errors = [msg | (msg, ok) <- checks, not ok]
      details = [msg ++ ": OK" | (msg, ok) <- checks, ok]
  in case verify743WireFixture of
    Left err -> FixtureResult False (registerSize reg) [err] details
    Right () -> FixtureResult (null errors) (registerSize reg) errors details

-- | Run fixture validation (general entry point)
runFixtureValidation :: FixtureResult
runFixtureValidation = run743WireFixture
