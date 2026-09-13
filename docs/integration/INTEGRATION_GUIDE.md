# ROSA + Async Daemon + Crypto-Todo Integration Guide

## Architecture Overview

This system implements the **separation of concerns** pattern for quantum compiler orchestration:

```
┌──────────────────────────────────────────────────┐
│ ROSA AGENT LAYER                                 │
│ (Reasoning, Planning, Tool Selection)            │
│                                                  │
│ "Build quantum-crypto-lang"                      │
│      ↓                                            │
│ Analyze dependency graph:                        │
│   • Wire → Gate → Operation → Circuit            │
│   • Lexer → Parser → Semantic → IR               │
│      ↓                                            │
│ Next executable todo: Wire model (todo #2)       │
│      ↓                                            │
│ Create ExecutionRequest                          │
└────────────────┬─────────────────────────────────┘
                 │ ExecutionRequest (typed message)
                 │ {
                 │   requestId: "req-20260912-001"
                 │   toolId: "qcl.build.wire"
                 │   operation: "Build Wire model..."
                 │   arguments: {module: "QCL.IR.Wire"}
                 │   priority: 3 (High)
                 │   deadline: 2026-09-12T14:35:00Z
                 │ }
                 ▼
┌──────────────────────────────────────────────────┐
│ ASYNC EXECUTION DAEMON                           │
│ (Policy, Scheduling, Execution Control)         │
│                                                  │
│ 1. RECEIVED                                      │
│    └─ Request received, queued                   │
│                                                  │
│ 2. VALIDATING                                    │
│    └─ Check: toolId in registry?                 │
│       "qcl.build.wire" ✓ found                  │
│                                                  │
│ 3. AUTHORIZED                                    │
│    └─ Enforce global policy:                     │
│       • Parameters valid? ✓                      │
│       • Resources available? ✓                   │
│       • No conflicts? ✓                          │
│       • Policy decision: ALLOWED                 │
│                                                  │
│ 4. QUEUED                                        │
│    └─ Acquire build lock, add to queue           │
│                                                  │
│ 5. RUNNING                                       │
│    └─ Execute: cabal build QCL.IR.Wire          │
│       Backend: ghc 9.2.8, cabal 3.6.2            │
│       stdout: "Building QCL.IR.Wire..."          │
│       stderr: "Warning: -Wcompat..."              │
│       exit code: 0 (SUCCESS)                     │
│                                                  │
│ 6. OBSERVING                                     │
│    └─ Collect telemetry:                         │
│       • Execution time: 12.3 seconds             │
│       • Warnings: 1                              │
│       • Errors: 0                                │
│       • Backend: ghc-cabal                       │
│                                                  │
│ 7. COMPLETED                                     │
│    └─ Generate ExecutionResponse                 │
└────────────────┬─────────────────────────────────┘
                 │ ExecutionResponse (typed message)
                 │ {
                 │   requestId: "req-20260912-001"
                 │   status: ExecutionCompleted
                 │   result: "Build succeeded"
                 │   error: null
                 │   telemetry: {
                 │     queueWaitTime: 0.1s
                 │     executionTime: 12.3s
                 │     resourceUsage: {cpuPercent: 45.2, memoryMB: 256}
                 │   }
                 │   startedAt: 2026-09-12T14:23:45Z
                 │   completedAt: 2026-09-12T14:23:57Z
                 │   backend: "ghc-cabal"
                 │ }
                 ▼
┌──────────────────────────────────────────────────┐
│ ROSA AGENT LAYER (again)                         │
│ (Observation Processing, Re-planning)            │
│                                                  │
│ Receive ExecutionResponse                        │
│      ↓                                            │
│ Process observation:                             │
│   • Status: ExecutionCompleted ✓                │
│   • Errors: none                                 │
│      ↓                                            │
│ Update crypto-todo library:                      │
│   • Todo #2 (Wire) → Completed                  │
│   • Mark at timestamp 14:23:57 with 12.3s time  │
│      ↓                                            │
│ Record audit log:                                │
│   • request_id: req-20260912-001                │
│   • todo_id: 2                                   │
│   • policy_decision: ALLOWED                    │
│   • final_outcome: TODO_2_COMPLETED             │
│      ↓                                            │
│ Resolve dependencies:                            │
│   • Todo #3 (Gate) now has dependency met       │
│   • Todo #3 now READY TO EXECUTE                │
│      ↓                                            │
│ Continue loop → Select todo #3                  │
│ Create ExecutionRequest for Gate model          │
└──────────────────────────────────────────────────┘
```

## Component Responsibilities

### ROSA Agent Layer

**Does:**
- Interprets natural language: "Build quantum-crypto-lang"
- Analyzes dependency DAG: What must complete first?
- Selects next executable tool: Gate model is ready
- Creates typed ExecutionRequest
- Receives ExecutionResponse
- Updates state based on observation
- Determines next action

**Does NOT:**
- Execute cabal builds
- Enforce policies
- Manage resource locks
- Collect telemetry
- Validate parameters (daemon does this)
- Store audit logs (daemon does this)

### Async Execution Daemon

**Does:**
- Receive ExecutionRequest from ROSA
- Validate request: Is toolId in registry?
- Enforce policy: Are parameters valid? Resources available?
- Acquire resource locks (prevent concurrent builds)
- Execute tool: Run cabal build
- Collect telemetry (time, resources, output)
- Generate audit log entry
- Return ExecutionResponse to ROSA

**Does NOT:**
- Reason about dependencies
- Plan next steps
- Interpret natural language
- Make tool selection decisions
- Perform LLM inference
- Understand quantum semantics

### Crypto-Todo Library

**Does:**
- Stores 18 todos with metadata
- Tracks todo status (pending, in_progress, completed, failed, blocked)
- Resolves dependencies (can we execute todo N?)
- Creates ExecutionRequest for a todo
- Records ExecutionResponse as audit log
- Tracks completion percentage
- Provides state monad for updates

**Used by:**
- ROSA layer: Query next executable todo, update on response
- Daemon: (doesn't directly use, but results flow back)

## Execution Flow: Todo #2 (Wire Model)

### Step 1: ROSA Queries Todo State

```haskell
st <- return $ initTodoState now

-- Get all todos
todos <- execState getAllTodos st

-- Find next executable
(nextTodo, st') <- runState getNextTodo st

case nextTodo of
  Just todo -> putStrLn $ "Next: " ++ content todo  -- "Build Wire model..."
  Nothing -> putStrLn "No pending todos"
```

### Step 2: ROSA Creates ExecutionRequest

```haskell
let req = ExecutionRequest
      { requestId = "req-20260912-001"
      , toolId = "qcl.build.wire"
      , operation = "Build Wire model with 743-wire support"
      , arguments = Map.fromList [("module", "QCL.IR.Wire")]
      , priority = 3  -- High priority
      , deadline = Just $ addUTCTime 60 now  -- 60 second timeout
      , authorizationContext = AuthContext
          { userId = "rosa-agent-001"
          , permissions = ["build", "compile"]
          , scope = "quantum-crypto-lang"
          , trustLevel = 5
          }
      , correlationId = "task-20260912-001"
      }

-- Send to daemon via channel
writeChan daemonRequestCh req
```

### Step 3: Daemon Receives & Validates

```haskell
-- Async daemon main loop
processRequestLoop orchestrator = forever $ do
  req <- readChan (requestQueue orchestrator)

  -- Validate
  case receiveAndValidate req of
    Left err -> do
      -- Policy blocked: return error response
      let response = ExecutionResponse
            { status = Rejected
            , error = Just err
            }
      writeChan (responseChannel orchestrator) response

    Right requestState -> do
      -- Execute
      case executeToolRequest req of
        Left err -> ...
        Right response -> writeChan (responseChannel orchestrator) response
```

### Step 4: Daemon Enforces Policy

```haskell
enforceGlobalPolicy :: ExecutionRequest -> IO PolicyValidation
enforceGlobalPolicy req = do
  -- Check tool registry
  toolValid <- toolInRegistry (toolId req)

  -- Validate parameters (wire_count between 1 and 4096)
  paramErrors <- validateParameters (toolId req) (arguments req)

  -- Check resource availability
  requiredResources <- getResourceRequirements (toolId req)
  resourcesAvailable <- checkResourceAvailability requiredResources

  return $ PolicyValidation (toolValid && null paramErrors) [] requiredResources
```

### Step 5: Daemon Executes Tool

```haskell
executeCabalBuild :: String -> String -> IO (ExitCode, String, String)
executeCabalBuild module' toolId' = do
  let cmd = "cabal build " ++ module'
  (exitCode, stdout, stderr) <- readProcessWithExitCode "sh" ["-c", cmd] ""
  return (exitCode, stdout, stderr)

-- Dispatch to appropriate backend
result <- case toolId req of
  "qcl.build.wire" -> executeCabalBuild "QCL.IR.Wire" "qcl.build.wire"
  ...
```

### Step 6: Daemon Returns ExecutionResponse

```haskell
let response = ExecutionResponse
      { requestId = requestId req
      , status = ExecutionCompleted  -- or ExecutionFailed
      , result = Just stdout
      , error = if null stderr then Nothing else Just stderr
      , telemetry = Telemetry
          { queueWaitTime = 0.1
          , executionTime = 12.3
          , resourceUsage = ResourceUsage 45.2 256 42
          , events = [...]
          }
      , startedAt = startTime
      , completedAt = endTime
      , backend = "ghc-cabal"
      }

writeChan (responseChannel orchestrator) response
```

### Step 7: ROSA Receives Response & Updates Todo

```haskell
-- ROSA receives response from daemon
response <- readChan daemonResponseCh

case status response of
  ExecutionCompleted -> do
    now <- getCurrentTime
    let executionTime = realToFrac $ diffUTCTime (completedAt response) (startedAt response)
    
    -- Mark todo as completed
    st'' <- execState (markCompleted 2 now (Just executionTime)) st'
    
    -- Record in audit log
    let log = ExecutionLog
          { logId = requestId response
          , logTodoId = 2
          , requestSent = req
          , responseReceived = response
          , policyDecision = Allowed
          , finalOutcome = "TODO_2_COMPLETED"
          , logTimestamp = now
          }
    st''' <- execState (recordExecution log) st''
    
    -- Check progress
    progress <- execState getCompletionPercentage st'''
    putStrLn $ "Progress: " ++ show progress ++ "% (1/18 complete)"

  ExecutionFailed -> do
    now <- getCurrentTime
    st'' <- execState (markFailed 2 now (show (error response))) st'
    putStrLn "Todo #2 failed, requires investigation"

  _ -> putStrLn "Todo #2 timeout or error"
```

### Step 8: ROSA Continues to Next Todo

```haskell
-- Resolve dependencies for next todo
(nextTodo, st'''') <- runState getNextTodo st'''

case nextTodo of
  Just todo -> do
    putStrLn $ "Next todo (#" ++ show (todoId todo) ++ "): " ++ content todo
    -- Create ExecutionRequest for todo #3 (Gate model)
    let req2 = createExecutionRequest todo "req-20260912-002" authCtx
    writeChan daemonRequestCh req2

  Nothing -> putStrLn "All todos complete or blocked!"
```

## Policy Enforcement in Daemon

### Global Policy Filter

Every ExecutionRequest must pass:

```
ExecutionRequest
      ↓
┌─────────────────────────────┐
│ GLOBAL POLICY FILTER        │
├─────────────────────────────┤
│ ✓ Tool in registry?         │ qcl.build.wire
│   {qcl.init, qcl.build.*, ...}
│                              │
│ ✓ Parameters valid?         │ module: "QCL.IR.Wire"
│   wire_count: [1..4096]     │
│                              │
│ ✓ Resources available?      │ BuildLock
│   No concurrent builds      │
│                              │
│ ✓ Authorization valid?      │ userId, permissions
│   Read/write access         │
│                              │
│ ✓ No safety violations?     │ No network access
│                              │
└─────────────────────────────┘
      ↓
AUTHORIZATION DECISION
      ↓
Allowed / DeniedSafety / DeniedAuth / DeniedResource
```

### Parameter Validation Rules

```haskell
-- For qcl.build.wire
wire_count: 1 to 4096
wire_id: 0 to 4095
canonical_mapping: bijective + 743-wire conformant

-- For qcl.build.gate
gate_type: PAULI_X | PAULI_Y | PAULI_Z | HADAMARD | CNOT | T | S | ...
gate_qubits: 1 to 3

-- For qcl.build.operation
operation_count: 1 to 65536
```

## Audit Trail

Complete reconstruction of execution:

```json
{
  "logId": "req-20260912-001",
  "logTodoId": 2,
  "requestSent": {
    "requestId": "req-20260912-001",
    "toolId": "qcl.build.wire",
    "operation": "Build Wire model...",
    "arguments": {"module": "QCL.IR.Wire"},
    "priority": 3,
    "deadline": "2026-09-12T14:35:00Z",
    "authorizationContext": {...}
  },
  "responseReceived": {
    "requestId": "req-20260912-001",
    "status": "ExecutionCompleted",
    "result": "Build succeeded...",
    "error": null,
    "telemetry": {
      "queueWaitTime": 0.1,
      "executionTime": 12.3,
      "resourceUsage": {...}
    },
    "startedAt": "2026-09-12T14:23:45Z",
    "completedAt": "2026-09-12T14:23:57Z",
    "backend": "ghc-cabal"
  },
  "policyDecision": "Allowed",
  "finalOutcome": "TODO_2_COMPLETED",
  "logTimestamp": "2026-09-12T14:23:57Z"
}
```

## Key Design Principles

1. **Separation of Concerns**: ROSA (reasoning) ≠ Daemon (execution)
2. **Deterministic Control**: LLM proposes, infrastructure decides
3. **Policy Before Execution**: Every action validated before running
4. **Explicit State Transitions**: No silent failures or ambiguous states
5. **Immutable Audit Trail**: Complete reconstruction possible
6. **Resource Ownership**: Explicit locks prevent conflicts
7. **Linear Type Discipline**: Quantum wires cannot be copied (no-cloning)
8. **Typed Messages**: ExecutionRequest/Response are strongly typed
9. **No Direct Execution**: Daemon never runs arbitrary commands
10. **Tool Registry**: Only whitelisted tools are executable

## Testing Integration

### Unit Test: Policy Enforcement

```haskell
test_policy_wire_count_valid = do
  let req = ExecutionRequest
        { toolId = "qcl.build.wire"
        , arguments = Map.fromList [("wire_count", "743")]
        , ...
        }
  result <- enforceGlobalPolicy req
  assertBool "wire_count 743 should be valid" (isValid result)

test_policy_wire_count_invalid = do
  let req = ExecutionRequest
        { toolId = "qcl.build.wire"
        , arguments = Map.fromList [("wire_count", "99999")]
        , ...
        }
  result <- enforceGlobalPolicy req
  assertBool "wire_count 99999 should be invalid" (not (isValid result))
```

### Integration Test: Full Flow

```haskell
test_full_flow = do
  st0 <- return $ initTodoState now
  
  -- Query next todo
  (mt1, st1) <- runState getNextTodo st0
  assertEqual "First todo should be Wire" (Just 2) (todoId <$> mt1)
  
  -- Create request
  let req = createExecutionRequest (fromJust mt1) "req-001" authCtx
  assertEqual "Tool should be qcl.build.wire" "qcl.build.wire" (toolId req)
  
  -- Validate request
  valid <- enforceGlobalPolicy req
  assertBool "Policy should approve" (isValid valid)
  
  -- Execute (would go to daemon in real system)
  -- ... simulate execution ...
  
  -- Update todo state
  st2 <- execState (markCompleted 2 now (Just 12.3)) st1
  completed <- execState (getTodosByStatus Completed) st2
  assertEqual "Should have 1 completed todo" 1 (length completed)
  
  -- Get next todo
  (mt3, st3) <- runState getNextTodo st2
  assertEqual "Next todo should be Gate" (Just 3) (todoId <$> mt3)
```

## Summary

This architecture implements clean separation:

- **ROSA**: Thinks (reasoning, planning, tool selection)
- **Daemon**: Does (execution, policy enforcement, resource control)
- **Crypto-Todo**: Tracks (state, dependencies, progress)

The result is:
- **Auditable**: Every action logged
- **Safe**: Policy enforced before execution
- **Deterministic**: Same input → same output
- **Scalable**: Multiple daemons, single ROSA orchestrator
- **Testable**: Each layer can be tested independently
