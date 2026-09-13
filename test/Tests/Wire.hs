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

-- | Wire module tests
-- Tests wire creation, state transitions, ownership checking,
-- allocation/deallocation, control relationships, and the 743-wire fixture.

module Tests.Wire (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import qualified Data.Set as Set

import QCL.IR.Wire

tests :: TestTree
tests = testGroup "Wire"
  [ testGroup "Wire creation"
      [ testCase "createWire produces correct wire ID" $ do
          let w = createWire 1 "wire_0001" Zero DataQubit "test.qpl" 1
          wireId w @?= WireId 1

      , testCase "createWire sets source name" $ do
          let w = createWire 5 "wire_0005" One Ancilla "test.qpl" 5
          sourceName w @?= SourceWireName "wire_0005"

      , testCase "createWire defaults to Allocated state" $ do
          let w = createWire 1 "wire_0001" Zero DataQubit "test.qpl" 1
          currentState w @?= Allocated

      , testCase "createWire records initial state" $ do
          let w0 = createWire 1 "w1" Zero DataQubit "test.qpl" 1
          let w1 = createWire 2 "w2" One DataQubit "test.qpl" 2
          initialState w0 @?= Zero
          initialState w1 @?= One

      , testCase "createWire records designation" $ do
          let wData = createWire 1 "w1" Zero DataQubit "test.qpl" 1
          let wAnc  = createWire 2 "w2" Zero Ancilla "test.qpl" 2
          let wDump = createWire 3 "w3" Zero DumpBit "test.qpl" 3
          designation wData @?= DataQubit
          designation wAnc  @?= Ancilla
          designation wDump @?= DumpBit

      , testCase "createWire is canonical" $ do
          let w = createWire 1 "w1" Zero DataQubit "test.qpl" 1
          canonical w @?= True

      , testCase "createWire initializes empty operations" $ do
          let w = createWire 1 "w1" Zero DataQubit "test.qpl" 1
          operations w @?= []
          measurements w @?= []
          controlledBy w @?= Set.empty
          controls w @?= Set.empty
      ]

  , testGroup "Wire state transitions"
      [ testCase "Allocated -> Live is valid" $ do
          let reg = createWireRegister 3
          case transitionWireState (WireId 1) Live 0 reg of
            Right reg' -> do
              let Just w = getWire (WireId 1) reg'
              currentState w @?= Live
            Left err -> assertFailure err

      , testCase "Allocated -> Transformed is valid" $ do
          let reg = createWireRegister 2
          case transitionWireState (WireId 1) Transformed 0 reg of
            Right reg' -> do
              let Just w = getWire (WireId 1) reg'
              currentState w @?= Transformed
            Left err -> assertFailure err

      , testCase "Live -> Measured is valid" $ do
          let reg = createWireRegister 2
          case transitionWireState (WireId 1) Live 0 reg of
            Right reg' -> case transitionWireState (WireId 1) Measured 1 reg' of
              Right reg'' -> do
                let Just w = getWire (WireId 1) reg''
                currentState w @?= Measured
              Left err -> assertFailure err
            Left err -> assertFailure err

      , testCase "Measured -> Consumed is valid" $ do
          let reg = createWireRegister 1
          let Right reg1 = transitionWireState (WireId 1) Live 0 reg
          let Right reg2 = transitionWireState (WireId 1) Measured 1 reg1
          case transitionWireState (WireId 1) Consumed 2 reg2 of
            Right reg3 -> do
              let Just w = getWire (WireId 1) reg3
              currentState w @?= Consumed
            Left err -> assertFailure err

      , testCase "state history is recorded" $ do
          let reg = createWireRegister 1
          let Right reg1 = transitionWireState (WireId 1) Live 0 reg
          let Right reg2 = transitionWireState (WireId 1) Measured 1 reg1
          let Just w = getWire (WireId 1) reg2
          stateHistory w @?= [Allocated, Live, Measured]

      , testCase "invalid transition Allocated -> Measured is rejected" $ do
          let reg = createWireRegister 1
          case transitionWireState (WireId 1) Measured 0 reg of
            Left _ -> return ()  -- Expected
            Right _ -> assertFailure "Should have rejected Allocated -> Measured"
      ]

  , testGroup "Ownership checking"
      [ testCase "Allocated wire can be operated on" $ do
          let reg = createWireRegister 1
          case checkOwnership (WireId 1) reg of
            Right () -> return ()
            Left err -> assertFailure err

      , testCase "Live wire can be operated on" $ do
          let reg = createWireRegister 1
          let Right reg' = transitionWireState (WireId 1) Live 0 reg
          case checkOwnership (WireId 1) reg' of
            Right () -> return ()
            Left err -> assertFailure err

      , testCase "Measured wire cannot be operated on" $ do
          let reg = createWireRegister 1
          let Right reg1 = transitionWireState (WireId 1) Live 0 reg
          let Right reg2 = transitionWireState (WireId 1) Measured 1 reg1
          case checkOwnership (WireId 1) reg2 of
            Left _ -> return ()  -- Expected: cannot operate on measured wire
            Right () -> assertFailure "Should reject operating on measured wire"

      , testCase "Consumed wire cannot be operated on" $ do
          let reg = createWireRegister 1
          let Right reg1 = transitionWireState (WireId 1) Consumed 0 reg
          case checkOwnership (WireId 1) reg1 of
            Left _ -> return ()
            Right () -> assertFailure "Should reject operating on consumed wire"

      , testCase "non-existent wire is rejected" $ do
          let reg = createWireRegister 1
          case checkOwnership (WireId 999) reg of
            Left _ -> return ()
            Right () -> assertFailure "Should reject non-existent wire"
      ]

  , testGroup "743-wire fixture"
      [ testCase "fixture has exactly 743 wires" $ do
          let reg = canonical743Wires
          registerSize reg @?= 743

      , testCase "fixture has 743 allocated, 0 consumed" $ do
          let reg = canonical743Wires
          totalAllocated reg @?= 743
          totalConsumed reg @?= 0

      , testCase "all wire IDs are present (1..743)" $ do
          let reg = canonical743Wires
          length (getAllWires reg) @?= 743

      , testCase "odd-indexed wires start as Zero" $ do
          let reg = canonical743Wires
          let Just w1 = getWire (WireId 1) reg
          let Just w3 = getWire (WireId 3) reg
          let Just w743 = getWire (WireId 743) reg
          initialState w1 @?= Zero
          initialState w3 @?= Zero
          initialState w743 @?= Zero

      , testCase "even-indexed wires start as One" $ do
          let reg = canonical743Wires
          let Just w2 = getWire (WireId 2) reg
          let Just w4 = getWire (WireId 4) reg
          let Just w742 = getWire (WireId 742) reg
          initialState w2 @?= One
          initialState w4 @?= One
          initialState w742 @?= One

      , testCase "verify743WireFixture passes" $ do
          case verify743WireFixture of
            Right () -> return ()
            Left err -> assertFailure err

      , testCase "fixture validation detects structural problems" $ do
          case validateWireRegister canonical743Wires of
            Right () -> return ()
            Left err -> assertFailure err
      ]

  , testGroup "Allocation and deallocation"
      [ testCase "allocateWire increases register size" $ do
          let reg = emptyWireRegister
          let (w, reg') = allocateWire DataQubit reg
          registerSize reg' @?= 1
          wireId w @?= WireId 1

      , testCase "multiple allocations produce sequential IDs" $ do
          let reg0 = emptyWireRegister
          let (w1, reg1) = allocateWire DataQubit reg0
          let (w2, reg2) = allocateWire DataQubit reg1
          let (w3, reg3) = allocateWire Ancilla reg2
          wireId w1 @?= WireId 1
          wireId w2 @?= WireId 2
          wireId w3 @?= WireId 3
          registerSize reg3 @?= 3

      , testCase "deallocateWire marks wire as Consumed" $ do
          let reg = createWireRegister 3
          case deallocateWire (WireId 2) 0 reg of
            Right reg' -> do
              let Just w = getWire (WireId 2) reg'
              currentState w @?= Consumed
              totalConsumed reg' @?= 1
            Left err -> assertFailure err

      , testCase "deallocateWire fails for non-existent wire" $ do
          let reg = createWireRegister 1
          case deallocateWire (WireId 999) 0 reg of
            Left _ -> return ()
            Right _ -> assertFailure "Should fail for non-existent wire"
      ]

  , testGroup "Control relationships"
      [ testCase "setControlRelationship links two wires" $ do
          let reg = createWireRegister 3
          case setControlRelationship (WireId 1) (WireId 2) reg of
            Right reg' -> do
              let Just w1 = getWire (WireId 1) reg'
              let Just w2 = getWire (WireId 2) reg'
              assertBool "Controller should list controlled wire"
                (Set.member (WireId 2) (controls w1))
              assertBool "Controlled should list controller wire"
                (Set.member (WireId 1) (controlledBy w2))
            Left err -> assertFailure err

      , testCase "setControlRelationship fails for non-existent wires" $ do
          let reg = createWireRegister 1
          case setControlRelationship (WireId 1) (WireId 999) reg of
            Left _ -> return ()
            Right _ -> assertFailure "Should fail for non-existent controlled wire"
      ]

  , testGroup "Wire queries"
      [ testCase "getWiresByDesignation filters correctly" $ do
          let reg = createWireRegister 5
          let dataWires = getWiresByDesignation DataQubit reg
          length dataWires @?= 5

      , testCase "getWiresByState returns correct wires" $ do
          let reg = createWireRegister 3
          let allocated = getWiresByState Allocated reg
          length allocated @?= 3
          let Right reg' = transitionWireState (WireId 1) Live 0 reg
          let live = getWiresByState Live reg'
          length live @?= 1
          let stillAlloc = getWiresByState Allocated reg'
          length stillAlloc @?= 2
      ]
  ]
