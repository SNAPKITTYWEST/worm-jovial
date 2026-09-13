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

-- | Async Execution Daemon for ROSA orchestration layer
-- This daemon receives ExecutionRequest from ROSA, validates, schedules, executes,
-- and returns ExecutionResponse with structured observations.
--
-- IMPORTANT: This daemon does NOT contain LLM reasoning.
-- This daemon does NOT perform autonomous planning.
-- This daemon is a pure execution service downstream of ROSA.
--
-- ROSA handles: natural-language interpretation, tool selection, agent reasoning
-- ASYNC DAEMON handles: validation, scheduling, execution, resource control, observation

module QCL.Daemon.AsyncExecutor where

import Data.Aeson
import Data.Time
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import System.Process
import System.Exit

import QCL.Crypto.TodoLibrary

-- | Main daemon orchestrator.
-- Note: Chan fields have no Show instance, so we provide a custom instance.
data DaemonOrchestrator = DaemonOrchestrator
  { requestQueue :: Chan ExecutionRequest
  , responseChannel :: Chan ExecutionResponse
  , activeRequests :: Map.Map String DaemonRequestState
  , completedRequests :: [ExecutionResponse]
  , daemonStartTime :: UTCTime
  }

instance Show DaemonOrchestrator where
  show orch = "DaemonOrchestrator { activeRequests = "
    ++ show (activeRequests orch)
    ++ ", completedRequests = "
    ++ show (length (completedRequests orch))
    ++ ", daemonStartTime = "
    ++ show (daemonStartTime orch)
    ++ " }"

-- | State of a request being processed
data DaemonRequestState = DaemonRequestState
  { daemonState :: DaemonState
  , drsRequest :: ExecutionRequest
  , policyApproval :: PolicyDecision
  , resourceLock :: Maybe String
  , drsStartTime :: Maybe UTCTime
  , drsObservations :: [String]
  } deriving (Show, Eq)

-- | Policy validation result
data PolicyValidation = PolicyValidation
  { isValid :: Bool
  , invalidReasons :: [String]
  , pvRequiredResources :: [String]
  } deriving (Show, Eq)

-- | Resource requirement
data ResourceRequirement
  = BuildLock
  | CompileLock
  | TestLock
  | VerifyLock
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ResourceRequirement
instance FromJSON ResourceRequirement

-- | Global policy enforcement
--
-- Policy must be enforced BEFORE execution regardless of ROSA request.
-- This is the deterministic control layer.
-- Uses early-return via nested checks for proper short-circuiting.
enforceGlobalPolicy :: ExecutionRequest -> IO PolicyValidation
enforceGlobalPolicy req = do
  let tool = reqToolId req
  let args = reqArguments req

  -- Check tool registry
  toolValid <- toolInRegistry tool
  if not toolValid
    then return $ PolicyValidation False ["Tool not in registry: " ++ tool] []
    else do
      -- Validate parameters
      paramErrors <- validateParameters tool args
      if not (null paramErrors)
        then return $ PolicyValidation False paramErrors []
        else do
          -- Check resource constraints
          resources <- getResourceRequirements tool
          resourcesAvailable <- checkResourceAvailability resources
          if not resourcesAvailable
            then return $ PolicyValidation False ["Required resources unavailable"] resources
            else
              -- All checks passed
              return $ PolicyValidation True [] resources

-- | Tool registry: Only these tools are allowed
toolInRegistry :: String -> IO Bool
toolInRegistry t = do
  let allowedTools = Set.fromList
        [ "qcl.init"
        , "qcl.build.wire"
        , "qcl.build.gate"
        , "qcl.build.operation"
        , "qcl.build.register"
        , "qcl.build.typesystem"
        , "qcl.build.ownership"
        , "qcl.build.lexer"
        , "qcl.build.parser"
        , "qcl.build.semantic"
        , "qcl.build.optimizer"
        , "qcl.build.equivalence"
        , "qcl.build.quipper"
        , "qcl.build.simulator"
        , "qcl.build.openqasm"
        , "qcl.build.driver"
        , "qcl.build.fixture"
        , "qcl.verify.all"
        , "qcl.verify.wire"
        , "qcl.verify.circuit"
        ]
  return $ Set.member t allowedTools

-- | Parameter constraints for each tool
validateParameters :: String -> Map.Map String String -> IO [String]
validateParameters tool args = do
  case tool of
    "qcl.build.wire" -> do
      case Map.lookup "wire_count" args of
        Just wc -> do
          let w = read wc :: Int
          if w >= 1 && w <= 4096
            then return []
            else return ["wire_count must be between 1 and 4096"]
        Nothing -> return []

    "qcl.build.gate" -> do
      case Map.lookup "gate_type" args of
        Just gt -> do
          let validGates = Set.fromList
                [ "PAULI_X", "PAULI_Y", "PAULI_Z", "HADAMARD", "CNOT", "T", "S"
                , "SQRT_X", "SQRT_Y", "SQRT_Z", "RX", "RY", "RZ", "CPHASE"
                , "SWAP", "ISWAP", "CCNOT"
                ]
          if Set.member gt validGates
            then return []
            else return ["Invalid gate_type: " ++ gt]
        Nothing -> return []

    "qcl.build.operation" -> do
      case Map.lookup "operation_count" args of
        Just oc -> do
          let o = read oc :: Int
          if o >= 1 && o <= 65536
            then return []
            else return ["operation_count must be between 1 and 65536"]
        Nothing -> return []

    _ -> return []

-- | Resource requirements for each tool
getResourceRequirements :: String -> IO [String]
getResourceRequirements tool = do
  case tool of
    "qcl.build.wire" -> return ["BuildLock"]
    "qcl.build.gate" -> return ["BuildLock"]
    "qcl.build.operation" -> return ["BuildLock"]
    "qcl.build.register" -> return ["BuildLock"]
    "qcl.build.typesystem" -> return ["BuildLock"]
    "qcl.build.ownership" -> return ["BuildLock"]
    "qcl.build.lexer" -> return ["BuildLock"]
    "qcl.build.parser" -> return ["BuildLock"]
    "qcl.build.semantic" -> return ["BuildLock"]
    "qcl.build.optimizer" -> return ["BuildLock"]
    "qcl.build.equivalence" -> return ["BuildLock"]
    "qcl.build.quipper" -> return ["BuildLock"]
    "qcl.build.simulator" -> return ["BuildLock"]
    "qcl.build.openqasm" -> return ["BuildLock"]
    "qcl.build.driver" -> return ["BuildLock"]
    "qcl.build.fixture" -> return ["BuildLock"]
    "qcl.verify.all" -> return ["VerifyLock"]
    "qcl.verify.wire" -> return ["VerifyLock"]
    "qcl.verify.circuit" -> return ["VerifyLock"]
    _ -> return []

-- | Check if required resources are available
checkResourceAvailability :: [String] -> IO Bool
checkResourceAvailability reqs = do
  -- In a real implementation, would check actual resource locks
  return $ not (null reqs)

-- | Request lifecycle state machine
--
-- State progression: RECEIVED -> VALIDATING -> AUTHORIZED -> QUEUED -> RUNNING -> OBSERVING -> COMPLETED
-- Failure states: REJECTED, CANCELLED, TIMEOUT, FAILED, BACKEND_ERROR, POLICY_BLOCKED

-- | Receive and validate request
receiveAndValidate :: ExecutionRequest -> IO (Either String DaemonRequestState)
receiveAndValidate req = do
  now <- getCurrentTime

  -- Step 1: RECEIVED
  let state1 = DaemonRequestState
        { daemonState = DaemonReceived
        , drsRequest = req
        , policyApproval = DeniedSafety "not yet evaluated"
        , resourceLock = Nothing
        , drsStartTime = Just now
        , drsObservations = ["Request received: " ++ reqId req]
        }

  -- Step 2: VALIDATING
  policyResult <- enforceGlobalPolicy req

  if not (isValid policyResult)
    then return $ Left $ "Policy blocked: " ++ unwords (invalidReasons policyResult)
    else do
      let state2 = state1
            { daemonState = DaemonValidating
            , drsObservations = drsObservations state1 ++
                ["Policy validation passed", "Parameters validated"]
            }

      -- Step 3: AUTHORIZED
      let policyDec = case policyResult of
            PolicyValidation True [] _ -> Allowed
            PolicyValidation False reasons _ -> DeniedSafety (unwords reasons)
            _ -> DeniedSafety "unknown"

      let state3 = state2
            { daemonState = DaemonAuthorized
            , policyApproval = policyDec
            , drsObservations = drsObservations state2 ++ ["Authorization passed"]
            }

      return $ Right state3

-- | Execute cabal build command
executeCabalBuild :: String -> String -> IO (ExitCode, String, String)
executeCabalBuild moduleName tool = do
  let cmd = "cabal build " ++ moduleName
  (exitCode, stdout, stderr) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  return (exitCode, stdout, stderr)

-- | Execute tool based on request
executeToolRequest :: ExecutionRequest -> IO (Either String ExecutionResponse)
executeToolRequest req = do
  let tool = reqToolId req
  startTime <- getCurrentTime

  -- Dispatch to appropriate backend
  result <- case tool of
    "qcl.init" -> do
      (code, out, err) <- readProcessWithExitCode "cabal" ["init", "--lib"] ""
      return (code, out, err)

    "qcl.build.wire" -> executeCabalBuild "QCL.IR.Wire" "qcl.build.wire"
    "qcl.build.gate" -> executeCabalBuild "QCL.IR.Gate" "qcl.build.gate"
    "qcl.build.operation" -> executeCabalBuild "QCL.IR.Operation" "qcl.build.operation"
    "qcl.build.register" -> executeCabalBuild "QCL.IR.Register" "qcl.build.register"
    "qcl.build.typesystem" -> executeCabalBuild "QCL.Type.System" "qcl.build.typesystem"
    "qcl.build.ownership" -> executeCabalBuild "QCL.Type.Ownership" "qcl.build.ownership"
    "qcl.build.lexer" -> executeCabalBuild "QCL.Syntax.Lexer" "qcl.build.lexer"
    "qcl.build.parser" -> executeCabalBuild "QCL.Syntax.Parser" "qcl.build.parser"
    "qcl.build.semantic" -> executeCabalBuild "QCL.Semantic.Analyzer" "qcl.build.semantic"
    "qcl.build.optimizer" -> executeCabalBuild "QCL.Optimization.Optimizer" "qcl.build.optimizer"
    "qcl.build.equivalence" -> executeCabalBuild "QCL.Verification.Equivalence" "qcl.build.equivalence"
    "qcl.build.quipper" -> executeCabalBuild "QCL.Backend.Quipper" "qcl.build.quipper"
    "qcl.build.simulator" -> executeCabalBuild "QCL.Backend.Simulator" "qcl.build.simulator"
    "qcl.build.openqasm" -> executeCabalBuild "QCL.Backend.OpenQASM" "qcl.build.openqasm"
    "qcl.build.driver" -> executeCabalBuild "QCL.Compiler.Driver" "qcl.build.driver"
    "qcl.build.fixture" -> executeCabalBuild "QCL.Compiler.Fixture" "qcl.build.fixture"

    "qcl.verify.all" -> do
      (code, out, err) <- readProcessWithExitCode "cabal" ["test"] ""
      return (code, out, err)

    _ -> return (ExitFailure 1, "", "Unknown tool: " ++ tool)

  endTime <- getCurrentTime
  let (exitCode, stdout, stderr) = result
  let execStatus = case exitCode of
        ExitSuccess -> ExecutionCompleted
        ExitFailure _ -> ExecutionFailed

  let tel = Telemetry
        { telQueueWait = 0
        , telExecTime = realToFrac (diffUTCTime endTime startTime)
        , telResourceUsage = ResourceUsage 0 0 0
        , telEvents =
            [ TelemetryEvent
                { eventType = "execution_start"
                , tevTimestamp = startTime
                , tevMessage = "Tool execution started"
                }
            , TelemetryEvent
                { eventType = "execution_complete"
                , tevTimestamp = endTime
                , tevMessage = "Tool execution completed"
                }
            ]
        }

  return $ Right ExecutionResponse
    { respRequestId = reqId req
    , respStatus = execStatus
    , respResult = Just stdout
    , respError = if null stderr then Nothing else Just stderr
    , respTelemetry = tel
    , respStartedAt = startTime
    , respCompletedAt = endTime
    , respBackend = "ghc-cabal"
    }

-- | Main daemon process
runDaemon :: IO ()
runDaemon = do
  requestCh <- newChan
  responseCh <- newChan
  now <- getCurrentTime

  let orchestrator = DaemonOrchestrator
        { requestQueue = requestCh
        , responseChannel = responseCh
        , activeRequests = Map.empty
        , completedRequests = []
        , daemonStartTime = now
        }

  -- Start request processor thread
  _ <- forkIO $ processRequestLoop orchestrator

  -- Keep daemon running
  forever $ threadDelay 1000000

-- | Process requests from ROSA in a loop
processRequestLoop :: DaemonOrchestrator -> IO ()
processRequestLoop orchestrator = forever $ do
  let inChan = requestQueue orchestrator
  let outChan = responseChannel orchestrator

  -- Wait for request
  req <- readChan inChan

  -- Validate request
  validResult <- receiveAndValidate req
  case validResult of
    Left err -> do
      now <- getCurrentTime
      let errorResponse = ExecutionResponse
            { respRequestId = reqId req
            , respStatus = Rejected
            , respResult = Nothing
            , respError = Just err
            , respTelemetry = Telemetry 0 0 (ResourceUsage 0 0 0) []
            , respStartedAt = now
            , respCompletedAt = now
            , respBackend = "daemon"
            }
      writeChan outChan errorResponse

    Right _requestState -> do
      -- Execute request
      execResult <- executeToolRequest req
      case execResult of
        Left err -> do
          now <- getCurrentTime
          let errorResponse = ExecutionResponse
                { respRequestId = reqId req
                , respStatus = BackendError
                , respResult = Nothing
                , respError = Just err
                , respTelemetry = Telemetry 0 0 (ResourceUsage 0 0 0) []
                , respStartedAt = now
                , respCompletedAt = now
                , respBackend = "daemon"
                }
          writeChan outChan errorResponse

        Right response -> do
          -- Return successful response
          writeChan outChan response
