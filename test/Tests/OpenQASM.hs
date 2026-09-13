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

-- | Tests for OpenQASM backend
-- Verifies correct conversion of circuits to OpenQASM 2.0 format

module Tests.OpenQASM
  ( runTests
  ) where

import QCL.Backend.OpenQASM
import QCL.IR.Circuit
import QCL.IR.Operation
import QCL.IR.Gate
import QCL.IR.Wire
import QCL.IR.Register

-- | Run all OpenQASM backend tests
runTests :: IO Bool
runTests = do
  putStrLn "Testing OpenQASM backend..."

  let tests =
        [ ("Header format", testHeaderFormat)
        , ("Register declarations", testRegisterDeclarations)
        , ("Single qubit gates", testSingleQubitGates)
        , ("Two qubit gates", testTwoQubitGates)
        , ("Measurements", testMeasurements)
        , ("Resets", testResets)
        , ("Barriers", testBarriers)
        , ("Validation", testValidation)
        ]

  results <- mapM runTest tests
  let passed = length $ filter id results
  putStrLn $ "  OpenQASM: " ++ show passed ++ "/" ++ show (length results) ++ " tests passed"
  return (all id results)

-- | Run a single test
runTest :: (String, IO Bool) -> IO Bool
runTest (name, action) = do
  result <- action
  let status = if result then "PASS" else "FAIL"
  putStrLn $ "    [" ++ status ++ "] " ++ name
  return result

-- | Test: Header format is correct
testHeaderFormat :: IO Bool
testHeaderFormat = do
  let circ = createCircuit (CircuitName "test") 5
  let output = exportCircuit circ
  let headerOK = "OPENQASM 2.0;" `elem` lines output
  let includeOK = "include \"qelib1.inc\";" `elem` lines output
  return (headerOK && includeOK)

-- | Test: Register declarations are generated correctly
testRegisterDeclarations :: IO Bool
testRegisterDeclarations = do
  let circ = createCircuit (CircuitName "test") 10
  let regs = registerDeclarations circ
  let qregOK = any ("qreg q[" `elem`) (map (take 8) regs)
  let cregOK = any ("creg c[" `elem`) (map (take 8) regs)
  return (qregOK && cregOK)

-- | Test: Single qubit gates are formatted correctly
testSingleQubitGates :: IO Bool
testSingleQubitGates = do
  let wires = [WireId 0]
  let hGate = formatUnaryGate H wires
  let xGate = formatUnaryGate X wires
  let zGate = formatUnaryGate Z wires
  let sgGate = formatUnaryGate S wires

  return $
    hGate == ["h q[0];"] &&
    xGate == ["x q[0];"] &&
    zGate == ["z q[0];"] &&
    sgGate == ["s q[0];"]

-- | Test: Two qubit gates are formatted correctly
testTwoQubitGates :: IO Bool
testTwoQubitGates = do
  let wires = [WireId 0, WireId 1]
  let cnotGate = formatBinaryGate CNOT wires
  let swapGate = formatBinaryGate SWAP wires

  return $
    cnotGate == ["cx q[0],q[1];"] &&
    swapGate == ["swap q[0],q[1];"]

-- | Test: Parametric gates include angles
testParametricGates :: IO Bool
testParametricGates = do
  let wires = [WireId 0]
  let rxGate = formatParametricGate (Rx 1.5707963267948966) wires
  let ryGate = formatParametricGate (Ry 3.141592653589793) wires

  return $ not (null rxGate) && not (null ryGate)

-- | Test: Measurements are formatted correctly
testMeasurements :: IO Bool
testMeasurements = do
  let opid = OperationId "meas1"
  let op = createMeasurementOperation opid [WireId 0] 0

  case opType op of
    MeasurementOperation ->
      let instr = operationToQASM (createEmptyCircuit (CircuitName "test")) op
      in return (not (null instr) && any ("measure" `elem`) (map (take 7) instr))
    _ -> return False

-- | Test: Reset operations are formatted correctly
testResets :: IO Bool
testResets = do
  let opid = OperationId "reset1"
  let op = createResetOperation opid [WireId 0] 0

  case opType op of
    ResetOperation ->
      let instr = operationToQASM (createEmptyCircuit (CircuitName "test")) op
      in return (not (null instr) && any ("reset" `elem`) (map (take 5) instr))
    _ -> return False

-- | Test: Barriers are formatted correctly
testBarriers :: IO Bool
testBarriers = do
  let opid = OperationId "barrier1"
  let op = createBarrierOperation opid [WireId 0, WireId 1] 0

  case opType op of
    BarrierOperation ->
      let instr = operationToQASM (createEmptyCircuit (CircuitName "test")) op
      in return (not (null instr) && any ("barrier" `elem`) (map (take 7) instr))
    _ -> return False

-- | Test: Validation accepts valid OpenQASM
testValidation :: IO Bool
testValidation = do
  let validProgram =
        unlines
          [ "OPENQASM 2.0;"
          , "include \"qelib1.inc\";"
          , "qreg q[5];"
          , "creg c[5];"
          , "h q[0];"
          , "cx q[0],q[1];"
          ]

  case validateOpenQASM validProgram of
    Right () -> return True
    Left _ -> return False

-- | Test: Validation rejects invalid OpenQASM
testInvalidOpenQASM :: IO Bool
testInvalidOpenQASM = do
  let invalidProgram = "INVALID 1.0;\n"

  case validateOpenQASM invalidProgram of
    Right () -> return False  -- Should have failed
    Left _ -> return True     -- Correctly rejected
