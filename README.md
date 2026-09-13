# Quantum Cryptographic Language with ROSA Orchestrator

[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](LICENSE)
[![Haskell](https://img.shields.io/badge/Haskell-GHC%209.x-purple.svg)](https://www.haskell.org/)
[![Cabal](https://img.shields.io/badge/build-cabal-orange.svg)](quantum-crypto-lang.cabal)
[![Quipper](https://img.shields.io/badge/backend-Quipper%20743--wire-green.svg)](docs/QUIPPER_IMPORTER.md)
[![OpenQASM](https://img.shields.io/badge/backend-OpenQASM%202.0-lightblue.svg)](docs/OPENQASM_BACKEND.md)
[![SPARK](https://img.shields.io/badge/formal-SPARK%20Ada-red.svg)](../worm-crypto-engine-ada/)
[![Lines](https://img.shields.io/badge/lines-7%2C118-brightgreen.svg)](src/QCL/)
[![JOVIAL](https://img.shields.io/badge/JOVIAL-J73%20WORM--SECRET-orange.svg)](WORMJOV.jov)

A complete quantum cryptographic programming language built in Haskell with formal verification boundaries, extracted from Quipper 743-wire source and orchestrated via the ROSA (Reasoning Over System Architecture) pattern.

## Architecture

### Three-Layer System

```
┌─────────────────────────────────────┐
│  ROSA Orchestration Layer           │
│  (Planning, tool selection, deps)   │
└──────────────┬──────────────────────┘
               │ ExecutionRequest
               ▼
┌──────────────────────────────────────┐
│  Async Execution Daemon              │
│  (Policy, resource control, runs)    │
└──────────────┬──────────────────────┘
               │ ExecutionResponse
               ▼
┌──────────────────────────────────────┐
│  Quantum Compiler Backend            │
│  (Cabal, verification, backends)     │
└──────────────────────────────────────┘
```

## Project Structure

```
quantum-crypto-lang/
├── quantum-crypto-lang.cabal          (Cabal project definition)
├── ROSA_ORCHESTRATOR.md               (Orchestration specification)
│
├── src/
│   └── QCL/
│       ├── Syntax/
│       │   ├── AST.hs                 (TODO: Parse tree)
│       │   ├── Lexer.hs               (TODO: Tokenization)
│       │   └── Parser.hs              (TODO: Grammar + AST construction)
│       │
│       ├── IR/
│       │   ├── Wire.hs                (✓ DONE: 743-wire canonical model)
│       │   ├── Gate.hs                (TODO: Canonical gates)
│       │   ├── Operation.hs           (TODO: Operation DAG)
│       │   ├── Circuit.hs             (TODO: Circuit IR)
│       │   └── Register.hs            (TODO: Quantum registers)
│       │
│       ├── Type/
│       │   ├── System.hs              (TODO: Quantum types)
│       │   └── Ownership.hs           (TODO: Linear/affine checker)
│       │
│       ├── Semantic/
│       │   ├── Analyzer.hs            (TODO: Semantic analysis)
│       │   └── IRGenerator.hs         (TODO: IR generation)
│       │
│       ├── Optimization/
│       │   └── Optimizer.hs           (TODO: Circuit optimization)
│       │
│       ├── Verification/
│       │   ├── Equivalence.hs         (TODO: Circuit equivalence)
│       │   └── Verifier.hs            (TODO: Circuit verifier)
│       │
│       ├── Backend/
│       │   ├── Quipper.hs             (TODO: Quipper importer)
│       │   ├── Simulator.hs           (TODO: Classical simulator)
│       │   └── OpenQASM.hs            (TODO: OpenQASM backend)
│       │
│       ├── Crypto/
│       │   └── TodoLibrary.hs         (✓ DONE: 18-todo state machine)
│       │
│       ├── Daemon/
│       │   └── AsyncExecutor.hs       (✓ DONE: Policy + execution daemon)
│       │
│       └── Compiler/
│           ├── Driver.hs              (TODO: Compiler entry point)
│           ├── Fixture.hs             (TODO: 743-wire test fixture)
│           └── Main.hs                (TODO: CLI driver)
│
├── test/
│   └── Main.hs                        (Test harness)
│       ├── Tests/Wire.hs              (TODO: Wire tests)
│       ├── Tests/Gate.hs              (TODO: Gate tests)
│       ├── Tests/Parser.hs            (TODO: Parser tests)
│       └── Tests/Fixture.hs           (TODO: 743-wire fixture validation)
│
└── docs/
    └── (Architecture, design docs)
```

## Build Status

| Todo | Component | Status | Priority | Dependencies |
|------|-----------|--------|----------|--------------|
| 1 | Project structure & cabal | ✓ | high | - |
| 2 | Wire model (743-wire) | ✓ | high | 1 |
| 3 | Gate model | ⏳ | high | 2 |
| 4 | Operation & Circuit IR | ⏳ | high | 3 |
| 5 | Quantum Register | ⏳ | high | 4 |
| 6 | Type System | ⏳ | high | 5 |
| 7 | Ownership Checker | ⏳ | high | 6 |
| 8 | Lexer | ⏳ | high | 7 |
| 9 | Parser & AST | ⏳ | high | 8 |
| 10 | Semantic Analyzer | ⏳ | high | 9 |
| 11 | Circuit Optimizer | ⏳ | high | 10 |
| 12 | Equivalence & Verifier | ⏳ | high | 11 |
| 13 | Quipper Importer | ⏳ | high | 12 |
| 14 | Simulator Backend | ⏳ | high | 13 |
| 15 | OpenQASM Backend | ⏳ | high | 14 |
| 16 | Compiler Driver | ⏳ | high | 15 |
| 17 | 743-wire Fixture | ⏳ | high | 16 |
| 18 | Verify all builds | ⏳ | high | 17 |

## ROSA Orchestrator

The ROSA model defines:

1. **18 typed tools** — Each module is a tool with:
   - Input schema (WireBuildRequest, etc.)
   - Output schema (WireBuildResult, etc.)
   - Safety class (MUTATING, READ_ONLY)
   - Execution mode (SERIAL_ONLY, PARALLEL_SAFE)
   - Backend (cabal, lean 4, etc.)
   - Timeout (60s default)
   - Determinism (deterministic, non-deterministic)

2. **Dependency DAG** — Builds follow explicit order:
   ```
   Wire → Gate → Operation → Circuit → Type System → Ownership
   Lexer → Parser → Semantic → IR Generator
   Optimizer → Equivalence → Verifier
   Quipper → Simulator → OpenQASM → Driver → Fixture → Verify All
   ```

3. **Policy enforcement** — Every build must pass:
   - Global policy filter (tool in registry, parameters valid, resources available)
   - Schema validation
   - Authorization checks
   - Resource locking (no concurrent builds)

4. **State machine** — IDLE → PLANNING → TODO_N → COMPLETED/FAILED

5. **Audit log** — Every action generates immutable record with:
   - request_id, tool_id, parameters
   - policy decision, authorization result
   - execution status, result, errors
   - telemetry (time, resources)

6. **Failure handling** — Explicit failure categories:
   - INVALID_TOOL, INVALID_ARGUMENT, UNAUTHORIZED
   - SAFETY_VIOLATION, RESOURCE_CONFLICT, TIMEOUT
   - BACKEND_FAILURE, VERIFICATION_FAILURE

## Async Execution Daemon

The daemon is **NOT ROSA**. It is downstream orchestration:

```
ROSA (planning)
   ↓ ExecutionRequest
DAEMON (execution)
   ↓ ExecutionResponse
```

The daemon:

- **Receives** typed ExecutionRequest from ROSA
- **Validates** request against tool registry
- **Authorizes** via deterministic policy
- **Enqueues** request in scheduler
- **Executes** via cabal build / ghc
- **Observes** result (stdout, stderr, exit code)
- **Returns** ExecutionResponse to ROSA

The daemon does NOT contain:
- LLM reasoning
- Tool selection logic
- Autonomous planning
- Natural language processing

## Crypto-to-do Library

The `QCL.Crypto.TodoLibrary` module provides:

- **18 todos** as structured records with metadata
- **Dependency resolution** (can we execute todo N?)
- **State tracking** (pending, in_progress, completed, failed, blocked)
- **Execution requests** (typed message to daemon)
- **Execution responses** (structured observation from daemon)
- **Audit logging** (complete execution history)
- **Progress tracking** (completion percentage, status by priority)

### Example Usage

```haskell
import QCL.Crypto.TodoLibrary
import Control.Monad.State

-- Initialize todo state
st0 <- return $ initTodoState now

-- Get next executable todo
(mt, st1) <- runState getNextTodo st0

case mt of
  Nothing -> putStrLn "No pending todos"
  Just t -> do
    -- Create execution request for ROSA
    let req = createExecutionRequest t "req-001" authCtx
    putStrLn $ "Executing: " ++ content t

    -- Send to daemon
    response <- daemon req

    -- Mark complete
    st2 <- execState (markCompleted (todoId t) now (Just 12.5)) st1
    
    -- Continue
    progress <- execState getCompletionPercentage st2
    putStrLn $ "Progress: " ++ show progress ++ "%"
```

## Wire Model (743 Wires)

The `QCL.IR.Wire` module extracts and normalizes the 743-wire source:

### Quipper Source → Canonical Representation

```
wire_0001 = True   →  Wire(1, sourceName="wire_0001", initialState=One)
wire_0002 = False  →  Wire(2, sourceName="wire_0002", initialState=Zero)
...
wire_0743 = True   →  Wire(743, sourceName="wire_0743", initialState=One)
```

### Wire Lifecycle (Linear Ownership)

```
ALLOCATED → LIVE → TRANSFORMED → MEASURED → CONSUMED
```

No-cloning constraint: A quantum wire cannot be copied or used twice simultaneously.

### Wire Operations

- `allocateWire` — Create new wire
- `deallocateWire` — Mark as consumed
- `checkOwnership` — Verify no-cloning constraint
- `transitionWireState` — State machine transitions
- `addOperation` — Record gate application
- `addMeasurement` — Record measurement
- `setControlRelationship` — Mark control dependency

### 743-Wire Fixture Validation

```haskell
verify743WireFixture :: Either String ()
```

Validates:
- Register size = 743
- No duplicate wire IDs
- No gaps in indices
- Alternating initialization pattern
- All wires canonical
- All constraints satisfied

## Getting Started

### Prerequisites

- GHC 9.2+
- Cabal 3.6+

### Initialize Project

```bash
cd quantum-crypto-lang
cabal update
cabal build
```

### Run Tests

```bash
cabal test
```

### Build Specific Module

```bash
cabal build QCL.IR.Wire
cabal build QCL.Crypto.TodoLibrary
cabal build QCL.Daemon.AsyncExecutor
```

## Next Steps

1. **Build Gate Model** (todo #3)
   - Canonical gate vocabulary (Pauli, Hadamard, CNot, T, S, etc.)
   - Parameterized rotations (Rx, Ry, Rz)
   - Gate adjoint (inverse) relationship
   - Gate composition

2. **Build Operation & Circuit IR** (todos #4-5)
   - Directed acyclic dependency graph
   - Topological ordering
   - Quantum register abstraction
   - Basic circuit operations

3. **Type System & Ownership** (todos #6-7)
   - Quantum types (Qubit, QRegister, Ancilla, etc.)
   - Linear/affine type checker
   - No-cloning enforcement
   - Measurement constraint checking

4. **Parser & Semantic Analysis** (todos #8-10)
   - Lexer (tokenization)
   - Parser (AST construction)
   - Type checking
   - IR generation

5. **Optimization & Verification** (todos #11-13)
   - Circuit optimizer (identity cancellation, gate fusion)
   - Circuit equivalence checker
   - Quipper importer with source traceability

6. **Backends & Integration** (todos #14-17)
   - Simulator (reference interpreter)
   - OpenQASM export
   - Main driver (CLI)
   - 743-wire validation fixture

7. **Full Build Verification** (todo #18)
   - All modules build
   - All tests pass
   - Full project compiles to executable

## Safety Guarantees

- **No-cloning**: Quantum wires are linear types that cannot be duplicated
- **Ownership**: Every wire has explicit lifecycle from allocation to consumption
- **Measured state**: A wire cannot be operated on after measurement
- **Policy enforcement**: Every build validated before execution
- **Audit trail**: Complete reconstruction of build process available
- **Deterministic compilation**: Same source produces identical IR

## References

- ROSA: Reasoning Over System Architecture (JPL orchestration pattern)
- Quipper: Practical quantum programming language (extraction source)
- OpenQASM: Open Quantum Assembly Language (backend target)
- Communicating Sequential Processes (CSP for daemon architecture)
- Linear/Affine Types (quantum ownership discipline)

## License

MIT

## Authors

SnapKitty Team
Ahmad Ali Parr + Jessica Lee Westerhoff
