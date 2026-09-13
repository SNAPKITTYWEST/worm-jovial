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

-- | Quipper Backend Tests

module Tests.Quipper (runTests) where

import QCL.Backend.Quipper
import QCL.IR.Gate (CanonicalGate(..))
import qualified Data.Map as Map

-- | Run Quipper tests
runTests :: IO Bool
runTests = do
  putStrLn "Testing Quipper Backend..."

  -- Wire parsing tests
  putStr "  - Wire parsing: "
  wireParseOk <- testWireParsing
  putStrLn $ if wireParseOk then "OK" else "FAILED"

  -- Gate mapping tests
  putStr "  - Gate mapping: "
  gateMappingOk <- testGateMapping
  putStrLn $ if gateMappingOk then "OK" else "FAILED"

  -- Import tests
  putStr "  - Circuit import: "
  importOk <- testCircuitImport
  putStrLn $ if importOk then "OK" else "FAILED"

  -- 743-wire fixture tests
  putStr "  - 743-wire fixture: "
  fixtureOk <- testFixture
  putStrLn $ if fixtureOk then "OK" else "FAILED"

  -- Pretty printing tests
  putStr "  - Pretty printing: "
  prettyOk <- testPrettyPrinting
  putStrLn $ if prettyOk then "OK" else "FAILED"

  let allPassed = wireParseOk && gateMappingOk && importOk && fixtureOk && prettyOk
  return allPassed

-- | Test wire parsing
testWireParsing :: IO Bool
testWireParsing = do
  case parseWireDefinition "wire_0001 = False" 1 of
    Right w -> do
      let isCorrect = qwName w == "wire_0001" && qwIndex w == 1 && not (qwInitialValue w)
      return isCorrect
    Left _ -> return False

-- | Test gate mapping
testGateMapping :: IO Bool
testGateMapping = do
  let h_ok = mapQuipperGateToCanonical "H" == Just H
  let x_ok = mapQuipperGateToCanonical "X" == Just X
  let s_ok = mapQuipperGateToCanonical "S" == Just S
  let t_ok = mapQuipperGateToCanonical "T" == Just T
  let unknown_ok = mapQuipperGateToCanonical "UNKNOWN" == Nothing
  return (h_ok && x_ok && s_ok && t_ok && unknown_ok)

-- | Test circuit import
testCircuitImport :: IO Bool
testCircuitImport = do
  let source = unlines
        [ "wire_0001 = False"
        , "wire_0002 = True"
        ]
  case importCircuit "test.qpl" source of
    Right imported ->
      let wires_ok = icWireCount imported == 2
          name_ok = icName imported == "quipper_2w"
          sourcemap_ok = Map.size (icSourceMap imported) == 2
      in return (wires_ok && name_ok && sourcemap_ok)
    Left _ -> return False

-- | Test 743-wire fixture validation
testFixture :: IO Bool
testFixture = do
  case verify743WireImport (ImportedCircuit
    { icName = "test"
    , icWires = []
    , icGates = []
    , icWireCount = 743
    , icGateCount = 0
    , icCircuit = undefined
    , icSourceMap = Map.empty
    , icImportErrors = []
    , icSourceFile = "test.qpl"
    }) of
    Left _ -> do
      -- Correctly rejected empty wires with 743 count
      return True
    Right () -> return False

-- | Test pretty printing
testPrettyPrinting :: IO Bool
testPrettyPrinting = do
  let source = unlines
        [ "wire_0001 = False"
        ]
  case importCircuit "test.qpl" source of
    Right imported ->
      let trace = prettyImportTrace imported
          sm = prettySourceMap (icSourceMap imported)
          pp = prettyQuipperCircuit imported
      in return (not (null trace) && not (null sm) && not (null pp))
    Left _ -> return False
