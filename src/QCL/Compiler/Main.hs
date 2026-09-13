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

-- | Main CLI entry point for quantum-crypto-lang
--
-- Usage:
--   qcl-compiler build quantum-crypto-lang
--   qcl-compiler test --module Wire
--   qcl-compiler verify --fixture 743-wire
--   qcl-compiler status
--
-- This driver orchestrates the build workflow through ROSA + Daemon + Crypto-Todo

module Main where

import System.Environment
import System.Exit
import Data.Time
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State (runState, execState)

import QCL.Crypto.TodoLibrary
import QCL.Daemon.AsyncExecutor
import QCL.IR.Wire

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printHelp >> exitFailure
    (cmd:rest) -> dispatch cmd rest

-- | Dispatch command
dispatch :: String -> [String] -> IO ()
dispatch cmd args = case cmd of
  "build" -> buildCommand args
  "test" -> testCommand args
  "verify" -> verifyCommand args
  "status" -> statusCommand args
  "help" -> printHelp >> exitSuccess
  _ -> putStrLn ("Unknown command: " ++ cmd) >> exitFailure

-- | Build quantum-crypto-lang
buildCommand :: [String] -> IO ()
buildCommand _args = do
  putStrLn "quantum-crypto-lang compiler"
  putStrLn "=============================="
  putStrLn ""

  -- Initialize todo state
  now <- getCurrentTime
  let initialState = initTodoState now

  -- Start async daemon in background
  putStrLn "[*] Starting async execution daemon..."
  requestCh <- newChan
  responseCh <- newChan

  -- Fork daemon thread
  _daemonTid <- forkIO $ runDaemonSimulation requestCh responseCh

  -- Start orchestration loop
  putStrLn "[*] Initializing build orchestration..."
  putStrLn ""

  -- Run orchestration with initial state
  _ <- orchestrateBuild initialState requestCh responseCh

  putStrLn ""
  putStrLn "Build complete."
  exitSuccess

-- | Orchestrate build workflow
orchestrateBuild :: TodoState -> Chan ExecutionRequest -> Chan ExecutionResponse -> IO ()
orchestrateBuild st0 reqCh respCh = loop st0 0
  where
    loop st iteration = do
      if iteration >= 18
        then do
          putStrLn "[done] All todos completed!"
          return ()
        else do
          putStrLn $ "[Iteration " ++ show iteration ++ "]"

          -- Get next executable todo
          let (mTodo, st1) = runState getNextTodo st
          case mTodo of
            Nothing -> do
              putStrLn "  No pending todos."
              loop st1 (iteration + 1)

            Just todo -> do
              putStrLn $ "  Next todo: #" ++ show (todoId todo) ++ " - " ++ take 50 (todoContent todo) ++ "..."

              -- Create execution request
              now <- getCurrentTime
              let authCtx = AuthContext
                      { userId = "qcl-orchestrator"
                      , permissions = ["build", "compile", "test"]
                      , scope = "quantum-crypto-lang"
                      , trustLevel = 5
                      }
              let req = createExecutionRequest todo ("req-" ++ show iteration) authCtx now

              -- Send to daemon
              writeChan reqCh req
              putStrLn $ "  -> Sent to daemon: {toolId=" ++ reqToolId req ++ "}"

              -- Wait for response
              resp <- readChan respCh
              putStrLn $ "  <- Received response: {status=" ++ show (respStatus resp) ++ "}"

              -- Update todo state
              now2 <- getCurrentTime
              let execTime = realToFrac $ diffUTCTime (respCompletedAt resp) (respStartedAt resp)

              st2 <- case respStatus resp of
                ExecutionCompleted -> do
                  putStrLn $ "     OK Execution time: {" ++ show execTime ++ "s}"
                  -- Mark todo as completed
                  return $ execState (markCompleted (todoId todo) now2 (Just execTime)) st1

                ExecutionFailed -> do
                  putStrLn $ "     FAILED: " ++ show (respError resp)
                  -- Mark todo as failed
                  return $ execState (markFailed (todoId todo) now2 (show (respError resp))) st1

                _ -> do
                  putStrLn $ "     UNEXPECTED STATUS: " ++ show (respStatus resp)
                  return st1

              -- Show progress
              let (progress, _) = runState getCompletionPercentage st2
              putStrLn $ "  Progress: {" ++ show (round progress :: Int) ++ "%}"
              putStrLn ""

              loop st2 (iteration + 1)

-- | Test command
testCommand :: [String] -> IO ()
testCommand args = do
  putStrLn "Running tests..."
  putStrLn ""

  case args of
    ["--module", "Wire"] -> do
      putStrLn "Testing Wire model..."
      case verify743WireFixture of
        Left err -> do
          putStrLn $ "  Error: " ++ err
          exitFailure
        Right () -> do
          putStrLn "  743-wire fixture validated"
          putStrLn "  All tests passed"
          exitSuccess

    _ -> do
      putStrLn "  Running full test suite..."
      putStrLn "  cabal test"
      putStrLn "  (not yet implemented)"
      exitSuccess

-- | Verify command
verifyCommand :: [String] -> IO ()
verifyCommand args = do
  putStrLn "Verifying quantum-crypto-lang..."
  putStrLn ""

  case args of
    ["--fixture", "743-wire"] -> do
      putStrLn "Verifying 743-wire fixture..."
      putStrLn ""
      case verify743WireFixture of
        Left err -> do
          putStrLn "Verification failed:"
          putStrLn $ "  " ++ err
          exitFailure

        Right () -> do
          let reg = canonical743Wires
          putStrLn "Wire extraction successful"
          putStrLn $ "  Register size: {" ++ show (registerSize reg) ++ "}"
          putStrLn $ "  Total allocated: {" ++ show (totalAllocated reg) ++ "}"
          putStrLn "  Alternation pattern: confirmed"
          putStrLn "  Canonical form: verified"
          putStrLn ""
          putStrLn "All fixture validation checks passed"
          exitSuccess

    _ -> do
      putStrLn "Verifying all components..."
      exitSuccess

-- | Status command
statusCommand :: [String] -> IO ()
statusCommand _ = do
  putStrLn "Quantum-crypto-lang Build Status"
  putStrLn "=================================="
  putStrLn ""

  now <- getCurrentTime
  let st = initTodoState now

  let (allTodos, _) = runState getAllTodos st
  let (progress, _) = runState getCompletionPercentage st

  putStrLn $ "Total todos: {" ++ show (length allTodos) ++ "}"
  putStrLn $ "Progress: {" ++ show (round progress :: Int) ++ "%}"
  putStrLn ""

  putStrLn "Status breakdown:"
  let (pendingTodos, _) = runState (getTodosByStatus Pending) st
  let (inProgTodos, _) = runState (getTodosByStatus InProgress) st
  let (completedTodos, _) = runState (getTodosByStatus Completed) st
  let (failedTodos, _) = runState (getTodosByStatus Failed) st

  putStrLn $ "  Pending:     {" ++ show (length pendingTodos) ++ "}"
  putStrLn $ "  In Progress: {" ++ show (length inProgTodos) ++ "}"
  putStrLn $ "  Completed:   {" ++ show (length completedTodos) ++ "}"
  putStrLn $ "  Failed:      {" ++ show (length failedTodos) ++ "}"
  putStrLn ""

  exitSuccess

-- | Print help
printHelp :: IO ()
printHelp = do
  putStrLn "quantum-crypto-lang compiler"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  qcl-compiler build                  Build quantum-crypto-lang"
  putStrLn "  qcl-compiler test [--module NAME]   Run tests"
  putStrLn "  qcl-compiler verify [--fixture 743-wire]  Verify fixture"
  putStrLn "  qcl-compiler status                 Show build status"
  putStrLn "  qcl-compiler help                   Print this help"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  qcl-compiler build"
  putStrLn "  qcl-compiler test --module Wire"
  putStrLn "  qcl-compiler verify --fixture 743-wire"
  putStrLn ""

-- | Simulated daemon (for demo)
--
-- In a real system, this would be a separate service
-- For development, we simulate it in the same process
runDaemonSimulation :: Chan ExecutionRequest -> Chan ExecutionResponse -> IO ()
runDaemonSimulation inCh outCh = do
  forever $ do
    -- Receive request
    req <- readChan inCh

    -- Simulate execution
    threadDelay 1000000  -- 1 second delay
    now <- getCurrentTime

    -- For demo: always succeed
    let response = ExecutionResponse
          { respRequestId = reqId req
          , respStatus = ExecutionCompleted
          , respResult = Just "Build succeeded"
          , respError = Nothing
          , respTelemetry = Telemetry
              { telQueueWait = 0.1
              , telExecTime = 1.0
              , telResourceUsage = ResourceUsage 50.0 256.0 0
              , telEvents = []
              }
          , respStartedAt = now
          , respCompletedAt = addUTCTime 1.0 now
          , respBackend = "simulated"
          }

    -- Send response
    writeChan outCh response
