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

-- | Gate module tests
-- Tests gate creation, adjoint computation, Pauli matrices,
-- gate library completeness, and CNOT decomposition.

module Tests.Gate (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Complex
import qualified Data.Map as Map
import qualified Data.Set as Set

import QCL.IR.Gate

tests :: TestTree
tests = testGroup "Gate"
  [ testGroup "Gate creation"
      [ testCase "createUnaryGate for each canonical gate" $ do
          let h = createUnaryGate (GateId "h1") H 0 0
          gateType h @?= UnaryGate H
          gateQubits h @?= 1
          targetQubits h @?= [0]
          isControlled h @?= False

      , testCase "createParametricGate stores angle" $ do
          let rx = createParametricGate (GateId "rx1") (Rx 1.5707) 0 0
          gateType rx @?= ParametricGate (Rx 1.5707)
          gateQubits rx @?= 1
          assertBool "should contain theta parameter"
            (Map.member "theta" (parameters rx))

      , testCase "createBinaryGate for CNOT" $ do
          let cnot = createBinaryGate (GateId "cx1") CNOT 0 0 1
          gateType cnot @?= BinaryGate CNOT
          gateQubits cnot @?= 2
          isControlled cnot @?= True
          controlQubits cnot @?= [0]
          targetQubits cnot @?= [1]

      , testCase "createBinaryGate for SWAP (not controlled)" $ do
          let sw = createBinaryGate (GateId "sw1") SWAP 0 0 1
          gateType sw @?= BinaryGate SWAP
          isControlled sw @?= False

      , testCase "createMeasurementGate is non-unitary" $ do
          let m = createMeasurementGate (GateId "m1") Computational 0 0
          isUnitary m @?= False

      , testCase "createResetGate is non-unitary" $ do
          let r = createResetGate (GateId "r1") ResetZero 0 0
          isUnitary r @?= False
      ]

  , testGroup "Adjoint computation"
      [ testCase "H is self-adjoint" $ do
          adjoint H @?= H

      , testCase "X is self-adjoint" $ do
          adjoint X @?= X

      , testCase "Y is self-adjoint" $ do
          adjoint Y @?= Y

      , testCase "Z is self-adjoint" $ do
          adjoint Z @?= Z

      , testCase "I is self-adjoint" $ do
          adjoint I @?= I

      , testCase "S adjoint is S_adjoint" $ do
          adjoint S @?= S_adjoint

      , testCase "S_adjoint adjoint is S" $ do
          adjoint S_adjoint @?= S

      , testCase "T adjoint is T_adjoint" $ do
          adjoint T @?= T_adjoint

      , testCase "T_adjoint adjoint is T" $ do
          adjoint T_adjoint @?= T

      , testCase "SqrtX adjoint is SqrtX_adjoint" $ do
          adjoint SqrtX @?= SqrtX_adjoint

      , testCase "double adjoint is identity" $ do
          adjoint (adjoint S) @?= S
          adjoint (adjoint T) @?= T
          adjoint (adjoint SqrtX) @?= SqrtX
          adjoint (adjoint SqrtY) @?= SqrtY
          adjoint (adjoint SqrtZ) @?= SqrtZ

      , testCase "rotation adjoint negates angle" $ do
          adjointRotation (Rx 1.0) @?= Rx (-1.0)
          adjointRotation (Ry 2.0) @?= Ry (-2.0)
          adjointRotation (Rz 3.0) @?= Rz (-3.0)
          adjointRotation (PhaseShift 0.5) @?= PhaseShift (-0.5)

      , testCase "CNOT is self-adjoint" $ do
          adjointTwoQubit CNOT @?= CNOT

      , testCase "iSWAP adjoint is iSWAP_adjoint" $ do
          adjointTwoQubit iSWAP @?= iSWAP_adjoint
          adjointTwoQubit iSWAP_adjoint @?= iSWAP

      , testCase "CCX is self-adjoint" $ do
          adjointThreeQubit CCX @?= CCX

      , testCase "CSwap is self-adjoint" $ do
          adjointThreeQubit CSwap @?= CSwap
      ]

  , testGroup "Pauli matrices"
      [ testCase "X matrix is correct" $ do
          let m = getPauliMatrix X
          -- X = [[0,1],[1,0]]
          m !! 0 !! 0 @?= 0
          m !! 0 !! 1 @?= 1
          m !! 1 !! 0 @?= 1
          m !! 1 !! 1 @?= 0

      , testCase "Z matrix is correct" $ do
          let m = getPauliMatrix Z
          -- Z = [[1,0],[0,-1]]
          m !! 0 !! 0 @?= 1
          m !! 0 !! 1 @?= 0
          m !! 1 !! 0 @?= 0
          m !! 1 !! 1 @?= (-1)

      , testCase "I matrix is identity" $ do
          let m = getPauliMatrix I
          m !! 0 !! 0 @?= 1
          m !! 0 !! 1 @?= 0
          m !! 1 !! 0 @?= 0
          m !! 1 !! 1 @?= 1

      , testCase "H matrix has correct entries" $ do
          let m = getPauliMatrix H
          let s = 1 / sqrt 2
          -- H = [[s,s],[s,-s]]
          assertBool "H(0,0) approx 1/sqrt(2)"
            (abs (realPart (m !! 0 !! 0) - s) < 1e-10)
          assertBool "H(1,1) approx -1/sqrt(2)"
            (abs (realPart (m !! 1 !! 1) + s) < 1e-10)

      , testCase "S matrix has correct phase" $ do
          let m = getPauliMatrix S
          -- S = [[1,0],[0,i]]
          m !! 0 !! 0 @?= 1
          m !! 1 !! 1 @?= (0 :+ 1)

      , testCase "all canonical gates have 2x2 matrices" $ do
          let allCanon = [I, X, Y, Z, H, S, S_adjoint, T, T_adjoint,
                          SqrtX, SqrtX_adjoint, SqrtY, SqrtY_adjoint,
                          SqrtZ, SqrtZ_adjoint]
          mapM_ (\g -> do
            let m = getPauliMatrix g
            length m @?= 2
            length (m !! 0) @?= 2
            length (m !! 1) @?= 2
            ) allCanon
      ]

  , testGroup "Gate library completeness"
      [ testCase "standard gate library passes completeness check" $ do
          case verifyStandardGateLibrary of
            Right () -> return ()
            Left err -> assertFailure err

      , testCase "standard library has all essential unary gates" $ do
          let lib = createStandardGateLibrary
          let essential = [I, X, Y, Z, H, S, T]
          mapM_ (\g -> assertBool ("Missing gate: " ++ show g)
                        (Set.member g (canonicalGates lib))) essential

      , testCase "empty library fails completeness" $ do
          case verifyGateLibraryCompleteness emptyGateLibrary of
            Left _ -> return ()
            Right () -> assertFailure "Empty library should fail completeness"

      , testCase "addGate increases gate count" $ do
          let lib = emptyGateLibrary
          let g = createUnaryGate (GateId "test") H 0 0
          let lib' = addGate g lib
          gateCount lib' @?= 1

      , testCase "getGate retrieves added gate" $ do
          let lib = emptyGateLibrary
          let gid = GateId "test_h"
          let g = createUnaryGate gid H 0 0
          let lib' = addGate g lib
          case getGate gid lib' of
            Just g' -> gateType g' @?= UnaryGate H
            Nothing -> assertFailure "Gate not found"
      ]

  , testGroup "CNOT decomposition"
      [ testCase "CNOT decomposes to H-CZ-H" $ do
          let cnot = createBinaryGate (GateId "cx") CNOT 0 0 1
          case decompose cnot of
            Right gates -> do
              length gates @?= 3
              -- First gate should be H on target
              gateType (gates !! 0) @?= UnaryGate H
              -- Second gate should be CZ
              gateType (gates !! 1) @?= BinaryGate CZ
              -- Third gate should be H on target
              gateType (gates !! 2) @?= UnaryGate H
            Left err -> assertFailure err

      , testCase "H decomposes to Rz-Rx-Rz" $ do
          let h = createUnaryGate (GateId "h") H 0 0
          case decompose h of
            Right gates -> do
              length gates @?= 3
              case gateType (gates !! 0) of
                ParametricGate (Rz _) -> return ()
                _ -> assertFailure "Expected Rz gate"
              case gateType (gates !! 1) of
                ParametricGate (Rx _) -> return ()
                _ -> assertFailure "Expected Rx gate"
            Left err -> assertFailure err

      , testCase "S decomposes to Rz(pi/2)" $ do
          let s = createUnaryGate (GateId "s") S 0 0
          case decompose s of
            Right gates -> do
              length gates @?= 1
              case gateType (gates !! 0) of
                ParametricGate (Rz angle) ->
                  assertBool "S decomposes to Rz(pi/2)"
                    (abs (angle - pi/2) < 1e-10)
                _ -> assertFailure "Expected Rz gate"
            Left err -> assertFailure err

      , testCase "T decomposes to Rz(pi/4)" $ do
          let t = createUnaryGate (GateId "t") T 0 0
          case decompose t of
            Right gates -> do
              length gates @?= 1
              case gateType (gates !! 0) of
                ParametricGate (Rz angle) ->
                  assertBool "T decomposes to Rz(pi/4)"
                    (abs (angle - pi/4) < 1e-10)
                _ -> assertFailure "Expected Rz gate"
            Left err -> assertFailure err

      , testCase "primitive gates decompose to themselves" $ do
          let x = createUnaryGate (GateId "x") X 0 0
          case decompose x of
            Right gates -> length gates @?= 1
            Left err -> assertFailure err
      ]

  , testGroup "Gate validation"
      [ testCase "valid unary gate passes validation" $ do
          let g = createUnaryGate (GateId "h") H 0 0
          case validateGate g of
            Right () -> return ()
            Left err -> assertFailure err

      , testCase "valid binary gate passes validation" $ do
          let g = createBinaryGate (GateId "cx") CNOT 0 0 1
          case validateGate g of
            Right () -> return ()
            Left err -> assertFailure err
      ]

  , testGroup "Gate commutativity"
      [ testCase "gates on different qubits can commute" $ do
          let g1 = createUnaryGate (GateId "h1") H 0 0
          let g2 = createUnaryGate (GateId "h2") H 0 1
          assertBool "H on q0 and H on q1 should commute"
            (canCommute g1 g2)

      , testCase "gates on same qubit cannot commute" $ do
          let g1 = createUnaryGate (GateId "h1") H 0 0
          let g2 = createUnaryGate (GateId "x1") X 0 0
          assertBool "H and X on same qubit should not commute"
            (not (canCommute g1 g2))
      ]

  , testGroup "Gate qubit count"
      [ testCase "unary gate count is 1" $ do
          gateQubitCount (UnaryGate H) @?= 1

      , testCase "parametric gate count is 1" $ do
          gateQubitCount (ParametricGate (Rx 0.5)) @?= 1

      , testCase "binary gate count is 2" $ do
          gateQubitCount (BinaryGate CNOT) @?= 2

      , testCase "ternary gate count is 3" $ do
          gateQubitCount (TernaryGate CCX) @?= 3

      , testCase "measurement gate count is 1" $ do
          gateQubitCount (MeasurementGate Computational) @?= 1
      ]
  ]
