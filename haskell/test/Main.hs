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

-- | Test suite for quantum-crypto-lang
-- Uses tasty framework for structured test execution.

module Main where

import Test.Tasty

import qualified Tests.Wire as Wire
import qualified Tests.Gate as Gate
import qualified Tests.Parser as Parser
import qualified Tests.Fixture as Fixture
import qualified Tests.OpenQASM as OpenQASM
import qualified Tests.Quipper as Quipper

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "quantum-crypto-lang"
  [ Wire.tests
  , Gate.tests
  , Parser.tests
  , Fixture.tests
  , OpenQASM.tests
  , Quipper.tests
  ]
