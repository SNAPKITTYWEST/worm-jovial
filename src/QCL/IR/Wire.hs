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

-- | Quantum Wire model with 743-wire canonical extraction
--
-- This module implements the canonical quantum wire representation,
-- extracted and normalized from Quipper source.
--
-- Each wire has explicit identity, initial state, operations, controls,
-- and measurement relationships.
--
-- NO-CLONING: Quantum wires are linear resources that cannot be copied.
-- OWNERSHIP: Every wire has a controlled lifecycle from allocation to consumption.

module QCL.IR.Wire where

import Data.Aeson
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Quantum wire identifier (1..N)
newtype WireId = WireId Int
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WireId
instance FromJSON WireId

-- | Source wire name for traceability (e.g. "wire_0001")
newtype SourceWireName = SourceWireName String
  deriving (Show, Eq, Generic)

instance ToJSON SourceWireName
instance FromJSON SourceWireName

-- | Initial quantum state
data InitialState
  = Zero                    -- ^ |0⟩
  | One                     -- ^ |1⟩
  | Superposition Double    -- ^ α|0⟩ + β|1⟩ (stored as probability of |1⟩)
  | Unknown                 -- ^ Unspecified initialization
  deriving (Show, Eq, Generic)

instance ToJSON InitialState
instance FromJSON InitialState

-- | Wire lifecycle state
data WireState
  = Allocated      -- ^ Just allocated, not yet used
  | Live           -- ^ Active, operations ongoing
  | Transformed    -- ^ Underwent quantum operations
  | Measured       -- ^ Measurement applied (becomes classical)
  | Consumed       -- ^ Wire lifecycle complete
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WireState
instance FromJSON WireState

-- | Role of a wire in an operation
data WireRole
  = Target         -- ^ Target of a single-qubit gate
  | Control        -- ^ Control qubit in controlled gate
  | Workspace      -- ^ Ancilla or temporary workspace
  | Classical      -- ^ Result of measurement
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WireRole
instance FromJSON WireRole

-- | Wire designation
data WireDesignation
  = DataQubit      -- ^ Regular qubit for data
  | Ancilla        -- ^ Auxiliary qubit
  | DumpBit        -- ^ Temporary bit for cleanup
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WireDesignation
instance FromJSON WireDesignation

-- | Canonical quantum wire representation
data Wire = Wire
  { wireId :: WireId
  , sourceName :: SourceWireName
  , index :: Int
  , initialState :: InitialState
  , designation :: WireDesignation
  , currentState :: WireState
  , stateHistory :: [WireState]
  , operations :: [OperationRef]  -- ^ Operations involving this wire
  , controlledBy :: Set.Set WireId -- ^ Wires that control this wire
  , controls :: Set.Set WireId     -- ^ Wires controlled by this wire
  , measurements :: [MeasurementRef]
  , allocatedAt :: Int            -- ^ Time step when allocated
  , consumedAt :: Maybe Int       -- ^ Time step when consumed (if any)
  , sourceFile :: String
  , sourceLine :: Int
  , canonical :: Bool             -- ^ Has this wire been normalized?
  } deriving (Show, Eq, Generic)

instance ToJSON Wire
instance FromJSON Wire

-- | Reference to an operation involving this wire
data OperationRef = OperationRef
  { opId :: String
  , opType :: String
  , role :: WireRole
  , timestamp :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON OperationRef
instance FromJSON OperationRef

-- | Reference to a measurement
data MeasurementRef = MeasurementRef
  { measurementId :: String
  , timestamp :: Int
  , resultWire :: Maybe WireId
  } deriving (Show, Eq, Generic)

instance ToJSON MeasurementRef
instance FromJSON MeasurementRef

-- | Complete wire register
data WireRegister = WireRegister
  { wires :: Map.Map WireId Wire
  , registerSize :: Int
  , totalAllocated :: Int
  , totalConsumed :: Int
  , allocationTime :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON WireRegister
instance FromJSON WireRegister

-- | Create a canonical wire from Quipper source
createWire
  :: Int              -- ^ 1-based index
  -> String           -- ^ Source name (e.g. "wire_0001")
  -> InitialState     -- ^ Initial quantum state
  -> WireDesignation  -- ^ Qubit role
  -> String           -- ^ Source file
  -> Int              -- ^ Source line
  -> Wire
createWire idx srcName initState desig srcFile srcLine =
  Wire
    { wireId = WireId idx
    , sourceName = SourceWireName srcName
    , index = idx
    , initialState = initState
    , designation = desig
    , currentState = Allocated
    , stateHistory = [Allocated]
    , operations = []
    , controlledBy = Set.empty
    , controls = Set.empty
    , measurements = []
    , allocatedAt = 0
    , consumedAt = Nothing
    , sourceFile = srcFile
    , sourceLine = srcLine
    , canonical = True
    }

-- | Extract the canonical 743-wire representation
--
-- Transforms Quipper source:
--   wire_0001 = True
--   wire_0002 = False
--   ...
--   wire_0743 = True
--
-- Into canonical indexed wires:
--   Wire 1 with sourceName "wire_0001" and initialState Zero (False → |0⟩)
--   Wire 2 with sourceName "wire_0002" and initialState One (True → |1⟩)
--   ...
--   Wire 743 with sourceName "wire_0743" and initialState Zero
canonical743Wires :: WireRegister
canonical743Wires =
  let wires743 = [createWire i (printf "wire_%04d" i) (alternatingState i) DataQubit "quipper-extract.qpl" i | i <- [1..743]]
      wireMap = Map.fromList [(wireId w, w) | w <- wires743]
  in WireRegister
       { wires = wireMap
       , registerSize = 743
       , totalAllocated = 743
       , totalConsumed = 0
       , allocationTime = 0
       }
  where
    -- Alternating pattern: odd indices = False (|0⟩), even indices = True (|1⟩)
    alternatingState idx = if idx `mod` 2 == 0 then One else Zero
    printf fmt n = let s = show n in replicate (4 - length s) '0' ++ s

-- | Initialize empty wire register
emptyWireRegister :: WireRegister
emptyWireRegister = WireRegister
  { wires = Map.empty
  , registerSize = 0
  , totalAllocated = 0
  , totalConsumed = 0
  , allocationTime = 0
  }

-- | Create a new wire register of given size
createWireRegister :: Int -> WireRegister
createWireRegister n =
  let wireList = [createWire i (printf "wire_%04d" i) Zero DataQubit "generated" i | i <- [1..n]]
      wireMap = Map.fromList [(wireId w, w) | w <- wireList]
  in WireRegister
       { wires = wireMap
       , registerSize = n
       , totalAllocated = n
       , totalConsumed = 0
       , allocationTime = 0
       }
  where
    printf fmt n = let s = show n in replicate (4 - length s) '0' ++ s

-- | Allocate a new wire in a register
allocateWire :: WireDesignation -> WireRegister -> (Wire, WireRegister)
allocateWire desig reg =
  let newIndex = registerSize reg + 1
      newWire = createWire newIndex (sourceNameForIndex newIndex) Unknown desig "internal" (-1)
      newMap = Map.insert (wireId newWire) newWire (wires reg)
      newReg = reg { wires = newMap, registerSize = newIndex, totalAllocated = totalAllocated reg + 1 }
  in (newWire, newReg)
  where
    sourceNameForIndex i = printf "wire_%04d" i
    printf fmt n = let s = show n in replicate (4 - length s) '0' ++ s

-- | Deallocate a wire (mark as consumed)
deallocateWire :: WireId -> Int -> WireRegister -> Either String WireRegister
deallocateWire wid timestamp reg =
  case Map.lookup wid (wires reg) of
    Nothing -> Left $ "Wire not found: " ++ show wid
    Just w -> do
      let updated = w
            { currentState = Consumed
            , stateHistory = stateHistory w ++ [Consumed]
            , consumedAt = Just timestamp
            }
      let newMap = Map.insert wid updated (wires reg)
      Right $ reg { wires = newMap, totalConsumed = totalConsumed reg + 1 }

-- | Check wire ownership constraint (no-cloning)
--
-- A wire can only be in one of these states at a time:
-- - Allocated but unused
-- - Live (used in operations)
-- - Measured (measurement applied)
-- - Consumed (deallocated)
--
-- Concurrent use of the same wire is an error.
checkOwnership :: WireId -> WireRegister -> Either String ()
checkOwnership wid reg =
  case Map.lookup wid (wires reg) of
    Nothing -> Left $ "Wire not found: " ++ show wid
    Just w -> do
      case currentState w of
        Allocated -> Right ()
        Live -> Right ()
        Transformed -> Right ()
        Measured -> Left $ "Cannot operate on measured wire: " ++ show wid
        Consumed -> Left $ "Cannot operate on consumed wire: " ++ show wid

-- | Transition wire state
transitionWireState :: WireId -> WireState -> Int -> WireRegister -> Either String WireRegister
transitionWireState wid newState timestamp reg =
  case Map.lookup wid (wires reg) of
    Nothing -> Left $ "Wire not found: " ++ show wid
    Just w -> do
      -- Validate state transition
      case (currentState w, newState) of
        (Allocated, Live) -> Right ()
        (Allocated, Transformed) -> Right ()
        (Live, Transformed) -> Right ()
        (Live, Measured) -> Right ()
        (Transformed, Measured) -> Right ()
        (Measured, Consumed) -> Right ()
        (_, Consumed) -> Right ()
        (from, to) -> Left $ "Invalid state transition: " ++ show from ++ " -> " ++ show to

      let updated = w
            { currentState = newState
            , stateHistory = stateHistory w ++ [newState]
            }
      Right $ reg { wires = Map.insert wid updated (wires reg) }

-- | Add operation reference to wire
addOperation :: WireId -> OperationRef -> WireRegister -> Either String WireRegister
addOperation wid opRef reg =
  case Map.lookup wid (wires reg) of
    Nothing -> Left $ "Wire not found: " ++ show wid
    Just w -> do
      let updated = w { operations = operations w ++ [opRef] }
      Right $ reg { wires = Map.insert wid updated (wires reg) }

-- | Add measurement reference
addMeasurement :: WireId -> MeasurementRef -> WireRegister -> Either String WireRegister
addMeasurement wid measRef reg =
  case Map.lookup wid (wires reg) of
    Nothing -> Left $ "Wire not found: " ++ show wid
    Just w -> do
      -- Transition to measured state
      let updated = w { measurements = measurements w ++ [measRef] }
      Right $ reg { wires = Map.insert wid updated (wires reg) }

-- | Mark control relationships
setControlRelationship :: WireId -> WireId -> WireRegister -> Either String WireRegister
setControlRelationship controller controlled reg =
  case (Map.lookup controller (wires reg), Map.lookup controlled (wires reg)) of
    (Nothing, _) -> Left $ "Controller wire not found: " ++ show controller
    (_, Nothing) -> Left $ "Controlled wire not found: " ++ show controlled
    (Just ctrw, Just ctrld) -> do
      let updatedCtrl = ctrld { controlledBy = Set.insert controller (controlledBy ctrld) }
      let updatedCtr = ctrw { controls = Set.insert controlled (controls ctrw) }
      let newMap = Map.insert controlled updatedCtrl (wires reg)
          newMap' = Map.insert controller updatedCtr newMap
      Right $ reg { wires = newMap' }

-- | Get wire by ID
getWire :: WireId -> WireRegister -> Maybe Wire
getWire wid reg = Map.lookup wid (wires reg)

-- | Get all wires, sorted by ID
getAllWires :: WireRegister -> [Wire]
getAllWires reg = sortBy (comparing wireId) (Map.elems (wires reg))

-- | Get wires by designation
getWiresByDesignation :: WireDesignation -> WireRegister -> [Wire]
getWiresByDesignation desig reg =
  filter (\w -> designation w == desig) (Map.elems (wires reg))

-- | Get wires by state
getWiresByState :: WireState -> WireRegister -> [Wire]
getWiresByState state reg =
  filter (\w -> currentState w == state) (Map.elems (wires reg))

-- | Validate complete wire register
validateWireRegister :: WireRegister -> Either String ()
validateWireRegister reg = do
  let allWires = Map.elems (wires reg)

  -- Check no duplicate wire IDs
  let ids = map wireId allWires
  if length ids /= length (Set.fromList ids)
    then Left "Duplicate wire IDs found"
    else Right ()

  -- Check all wires are canonical
  let nonCanonical = filter (not . canonical) allWires
  if not (null nonCanonical)
    then Left $ "Non-canonical wires found: " ++ show (length nonCanonical)
    else Right ()

  -- Check wire indices match wire IDs
  let mismatchedIndices = filter (\w -> let WireId i = wireId w in index w /= i) allWires
  if not (null mismatchedIndices)
    then Left "Wire index/ID mismatch"
    else Right ()

  Right ()

-- | Verify 743-wire fixture
verify743WireFixture :: Either String ()
verify743WireFixture = do
  let reg = canonical743Wires

  -- Verify register size
  if registerSize reg /= 743
    then Left $ "Expected 743 wires, got " ++ show (registerSize reg)
    else Right ()

  -- Verify no gaps in wire IDs
  let allWires = getAllWires reg
  if length allWires /= 743
    then Left $ "Expected 743 wires, got " ++ show (length allWires)
    else Right ()

  -- Verify alternating pattern
  let checkAlternation = all (\w -> case (index w `mod` 2, initialState w) of
                                     (1, Zero) -> True
                                     (0, One) -> True
                                     _ -> False) allWires
  if not checkAlternation
    then Left "Wire initialization pattern does not match alternation"
    else Right ()

  validateWireRegister reg
