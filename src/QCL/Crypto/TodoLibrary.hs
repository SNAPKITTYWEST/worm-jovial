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

module QCL.Crypto.TodoLibrary where

import Data.Aeson
import Data.Time
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Monad.State
import Data.List (sortBy, (\\))
import Data.Ord (comparing)

-- | Todo status in the quantum-crypto-lang build lifecycle
data TodoStatus
  = InProgress
  | Pending
  | Completed
  | Failed
  | Blocked
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TodoStatus
instance FromJSON TodoStatus

-- | Priority level for todos
data Priority = Critical | High | Medium | Low
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Priority
instance FromJSON Priority

-- | A single todo in the crypto build workflow
-- All fields prefixed with @todo@ for Haskell2010 compatibility.
data Todo = Todo
  { todoId :: Int
  , todoContent :: String
  , todoStatus :: TodoStatus
  , todoPriority :: Priority
  , todoDependencies :: [Int]      -- ^ IDs of todos that must complete first
  , todoCreatedAt :: UTCTime
  , todoUpdatedAt :: UTCTime
  , todoCompletedAt :: Maybe UTCTime
  , todoModuleName :: String
  , todoToolId :: String
  , todoExecTime :: Maybe Double   -- ^ seconds
  , todoError :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON Todo
instance FromJSON Todo

-- | Execution request sent to async daemon.
-- All fields prefixed with @req@ for Haskell2010 compatibility.
data ExecutionRequest = ExecutionRequest
  { reqId :: String
  , reqToolId :: String
  , reqOperation :: String
  , reqArguments :: Map.Map String String
  , reqPriority :: Int
  , reqDeadline :: Maybe UTCTime
  , reqAuthContext :: AuthContext
  , reqCorrelationId :: String
  } deriving (Show, Eq, Generic)

instance ToJSON ExecutionRequest
instance FromJSON ExecutionRequest

-- | Response from async daemon.
-- All fields prefixed with @resp@ for Haskell2010 compatibility.
data ExecutionResponse = ExecutionResponse
  { respRequestId :: String
  , respStatus :: ExecutionStatus
  , respResult :: Maybe String
  , respError :: Maybe String
  , respTelemetry :: Telemetry
  , respStartedAt :: UTCTime
  , respCompletedAt :: UTCTime
  , respBackend :: String
  } deriving (Show, Eq, Generic)

instance ToJSON ExecutionResponse
instance FromJSON ExecutionResponse

-- | Execution status returned by daemon
data ExecutionStatus
  = Received
  | Validating
  | Authorized
  | Queued
  | Running
  | Observing
  | ExecutionCompleted
  | Rejected
  | Cancelled
  | ExecutionTimeout
  | ExecutionFailed
  | BackendError
  | PolicyBlocked
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ExecutionStatus
instance FromJSON ExecutionStatus

-- | Authorization context for policy enforcement
data AuthContext = AuthContext
  { userId :: String
  , permissions :: [String]
  , scope :: String
  , trustLevel :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON AuthContext
instance FromJSON AuthContext

-- | Telemetry collected during execution.
-- Fields prefixed with @tel@ to avoid collision with Todo.executionTime.
data Telemetry = Telemetry
  { telQueueWait :: Double
  , telExecTime :: Double
  , telResourceUsage :: ResourceUsage
  , telEvents :: [TelemetryEvent]
  } deriving (Show, Eq, Generic)

instance ToJSON Telemetry
instance FromJSON Telemetry

-- | Resource usage during execution
data ResourceUsage = ResourceUsage
  { cpuPercent :: Double
  , memoryMB :: Double
  , diskIOOps :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ResourceUsage
instance FromJSON ResourceUsage

-- | Individual telemetry event
data TelemetryEvent = TelemetryEvent
  { eventType :: String
  , tevTimestamp :: UTCTime
  , tevMessage :: String
  } deriving (Show, Eq, Generic)

instance ToJSON TelemetryEvent
instance FromJSON TelemetryEvent

-- | Todo database state
data TodoState = TodoState
  { todos :: Map.Map Int Todo
  , nextId :: Int
  , lastUpdated :: UTCTime
  , executionLog :: [ExecutionLog]
  } deriving (Show, Eq)

-- | Execution audit log entry
data ExecutionLog = ExecutionLog
  { logId :: String
  , logTodoId :: Int
  , requestSent :: ExecutionRequest
  , responseReceived :: ExecutionResponse
  , policyDecision :: PolicyDecision
  , finalOutcome :: String
  , logTimestamp :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ExecutionLog
instance FromJSON ExecutionLog

-- | Policy decision from daemon
data PolicyDecision
  = Allowed
  | DeniedSafety String
  | DeniedAuth String
  | DeniedResource String
  deriving (Show, Eq, Generic)

instance ToJSON PolicyDecision
instance FromJSON PolicyDecision

-- | Dependency resolution result
data DependencyResolution = DependencyResolution
  { canExecute :: Bool
  , blockedBy :: [Int]
  , readyToExecute :: [Int]
  } deriving (Show, Eq)

-- | Async daemon state
data DaemonState
  = DaemonReceived
  | DaemonValidating
  | DaemonAuthorized
  | DaemonQueued
  | DaemonRunning
  | DaemonObserving
  | DaemonCompleted
  deriving (Show, Eq, Ord, Generic)

instance ToJSON DaemonState
instance FromJSON DaemonState

-- | The 18 quantum-crypto-lang todos
quantumCryptoLangTodos :: [Todo]
quantumCryptoLangTodos =
  [ Todo 1 "Create project structure and cabal file for quantum-crypto-lang" InProgress High []
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "ProjectStructure" "qcl.init" Nothing Nothing

  , Todo 2 "Build Wire model with 743-wire extraction and canonical mapping" Pending High [1]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.IR.Wire" "qcl.build.wire" Nothing Nothing

  , Todo 3 "Build Gate model with canonical gate vocabulary" Pending High [2]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.IR.Gate" "qcl.build.gate" Nothing Nothing

  , Todo 4 "Build Operation and Circuit IR (directed dependency graph)" Pending High [3]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.IR.Operation" "qcl.build.operation" Nothing Nothing

  , Todo 5 "Build Quantum Register model" Pending High [4]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.IR.Register" "qcl.build.register" Nothing Nothing

  , Todo 6 "Build Type System with quantum types" Pending High [5]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Type.System" "qcl.build.typesystem" Nothing Nothing

  , Todo 7 "Build Linear/Affine Ownership Checker" Pending High [6]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Type.Ownership" "qcl.build.ownership" Nothing Nothing

  , Todo 8 "Build Lexer for quantum cryptographic language" Pending High [7]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Syntax.Lexer" "qcl.build.lexer" Nothing Nothing

  , Todo 9 "Build Parser and AST" Pending High [8]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Syntax.Parser" "qcl.build.parser" Nothing Nothing

  , Todo 10 "Build Semantic Analyzer and IR Generator" Pending High [9]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Semantic.Analyzer" "qcl.build.semantic" Nothing Nothing

  , Todo 11 "Build Circuit Optimizer" Pending High [10]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Optimization.Optimizer" "qcl.build.optimizer" Nothing Nothing

  , Todo 12 "Build Circuit Equivalence and Verifier" Pending High [11]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Verification.Equivalence" "qcl.build.equivalence" Nothing Nothing

  , Todo 13 "Build Quipper Importer with source traceability" Pending High [12]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Backend.Quipper" "qcl.build.quipper" Nothing Nothing

  , Todo 14 "Build Simulator Backend" Pending High [13]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Backend.Simulator" "qcl.build.simulator" Nothing Nothing

  , Todo 15 "Build OpenQASM Backend" Pending High [14]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Backend.OpenQASM" "qcl.build.openqasm" Nothing Nothing

  , Todo 16 "Build Compiler Main driver" Pending High [15]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Compiler.Driver" "qcl.build.driver" Nothing Nothing

  , Todo 17 "Build 743-wire validation test fixture" Pending High [16]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Compiler.Fixture" "qcl.build.fixture" Nothing Nothing

  , Todo 18 "Verify full project builds and all tests pass" Pending High [17]
      (read "2026-09-12 00:00:00 UTC") (read "2026-09-12 00:00:00 UTC") Nothing
      "QCL.Compiler" "qcl.verify.all" Nothing Nothing
  ]

-- | Initialize todo state
initTodoState :: UTCTime -> TodoState
initTodoState now = TodoState
  { todos = Map.fromList [(todoId t, t) | t <- quantumCryptoLangTodos]
  , nextId = 19
  , lastUpdated = now
  , executionLog = []
  }

-- | Type for state monad operations
type TodoM a = State TodoState a

-- | Get a todo by ID
getTodo :: Int -> TodoM (Maybe Todo)
getTodo tid = do
  st <- get
  return $ Map.lookup tid (todos st)

-- | Resolve dependencies: can we execute this todo?
resolveDependencies :: Int -> TodoM DependencyResolution
resolveDependencies tid = do
  st <- get
  case Map.lookup tid (todos st) of
    Nothing -> return $ DependencyResolution False [tid] []
    Just t -> do
      let deps = todoDependencies t
      depStatuses <- mapM getTodo deps
      let completedIds = [todoId td | Just td <- depStatuses, todoStatus td == Completed]
      let failedIds    = [todoId td | Just td <- depStatuses, todoStatus td == Failed]
      if not (null failedIds)
        then return $ DependencyResolution False failedIds []
        else if length completedIds == length deps
          then return $ DependencyResolution True [] [tid]
          else return $ DependencyResolution False (deps \\ completedIds) []

-- | Get next executable todo
getNextTodo :: TodoM (Maybe Todo)
getNextTodo = do
  st <- get
  let pending = Map.filter (\t -> todoStatus t == Pending) (todos st)
  results <- mapM (\t -> (todoId t,) <$> resolveDependencies (todoId t)) (Map.elems pending)
  let executable = [t | (tid, res) <- results, canExecute res, Just t <- [Map.lookup tid (todos st)]]
  case executable of
    [] -> return Nothing
    (t:_) -> return $ Just t

-- | Mark todo as in progress.
-- Accepts a UTCTime parameter (pure state operation, no IO).
markInProgress :: Int -> UTCTime -> TodoM ()
markInProgress tid now = do
  st <- get
  case Map.lookup tid (todos st) of
    Nothing -> return ()
    Just t -> do
      let updated = t { todoStatus = InProgress, todoUpdatedAt = now }
      put $ st { todos = Map.insert tid updated (todos st), lastUpdated = now }

-- | Mark todo as completed
markCompleted :: Int -> UTCTime -> Maybe Double -> TodoM ()
markCompleted tid now execTime = do
  st <- get
  case Map.lookup tid (todos st) of
    Nothing -> return ()
    Just t -> do
      let updated = t
            { todoStatus = Completed
            , todoUpdatedAt = now
            , todoCompletedAt = Just now
            , todoExecTime = execTime
            }
      put $ st { todos = Map.insert tid updated (todos st), lastUpdated = now }

-- | Mark todo as failed
markFailed :: Int -> UTCTime -> String -> TodoM ()
markFailed tid now err = do
  st <- get
  case Map.lookup tid (todos st) of
    Nothing -> return ()
    Just t -> do
      let updated = t
            { todoStatus = Failed
            , todoUpdatedAt = now
            , todoError = Just err
            }
      put $ st { todos = Map.insert tid updated (todos st), lastUpdated = now }

-- | Get completion percentage
getCompletionPercentage :: TodoM Double
getCompletionPercentage = do
  st <- get
  let total = Map.size (todos st)
  let done = length $ filter (\t -> todoStatus t == Completed) (Map.elems (todos st))
  return $ if total == 0 then 0 else fromIntegral done / fromIntegral total * 100

-- | Get all todos sorted by priority and ID
getAllTodos :: TodoM [Todo]
getAllTodos = do
  st <- get
  return $ sortBy (comparing todoId) (Map.elems (todos st))

-- | Get todos by status
getTodosByStatus :: TodoStatus -> TodoM [Todo]
getTodosByStatus s = do
  st <- get
  return $ filter (\t -> todoStatus t == s) (Map.elems (todos st))

-- | Create execution request for a todo.
-- Pure function: accepts a UTCTime so no IO is needed.
createExecutionRequest :: Todo -> String -> AuthContext -> UTCTime -> ExecutionRequest
createExecutionRequest t correlId authCtx now =
  ExecutionRequest
    { reqId = "req-" ++ show (todoId t)
    , reqToolId = todoToolId t
    , reqOperation = todoContent t
    , reqArguments = Map.fromList
        [ ("module", todoModuleName t)
        , ("todo_id", show (todoId t))
        ]
    , reqPriority = case todoPriority t of
        Critical -> 4
        High -> 3
        Medium -> 2
        Low -> 1
    , reqDeadline = Just $ addUTCTime 60 now  -- 60 second timeout
    , reqAuthContext = authCtx
    , reqCorrelationId = correlId
    }

-- | Record execution in audit log
recordExecution :: ExecutionLog -> TodoM ()
recordExecution entry = do
  st <- get
  put $ st { executionLog = entry : executionLog st }

-- | Get execution history for a todo
getExecutionHistory :: Int -> TodoM [ExecutionLog]
getExecutionHistory tid = do
  st <- get
  return $ filter (\entry -> logTodoId entry == tid) (executionLog st)
