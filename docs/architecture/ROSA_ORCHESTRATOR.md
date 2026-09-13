# ROSA Orchestrator for Quantum-Crypto-Lang

## Orchestration Model

```
REQUEST (Build quantum-crypto-lang)
   ↓
REASON (What needs to be done? 18 modules)
   ↓
PLAN (Dependency graph: Wire → Gate → Operation → Circuit → Type System → ...)
   ↓
SELECT ACTION (Next todo in dependency order)
   ↓
EXECUTE TOOL (Build module with cabal)
   ↓
OBSERVE RESULT (Module builds? Tests pass? Verification?)
   ↓
VALIDATE (Did todo complete? Can we move to next dependency?)
   ↓
CONTINUE / COMPLETE / FAIL
```

## Tool Registry

Every module build is a typed tool:

```
Tool: qcl.build.wire
  Input: WireBuildRequest
  Output: WireBuildResult
  Safety: MUTATING (modifies filesystem)
  Execution: SERIAL_ONLY
  Backend: Rust daemon (ghc + cabal)
  Timeout: 60s
  Determinism: Deterministic (idempotent)

Tool: qcl.verify.wire
  Input: WireVerifyRequest
  Output: WireVerifyResult
  Safety: READ_ONLY
  Execution: PARALLEL_SAFE
  Backend: Lean 4 verification
  Timeout: 30s
  Determinism: Deterministic

Tool: qcl.test.fixture
  Input: FixtureRequest
  Output: FixtureResult
  Safety: MUTATING
  Execution: SERIAL_ONLY
  Backend: Rust daemon (cargo test)
  Timeout: 120s
  Determinism: Non-deterministic (may vary)
```

## Dependency DAG

```
1. Project Structure (in_progress)
   ↓
2. Wire Model ──────┐
   ↓                │
3. Gate Model ──────┤
   ↓                │
4. Operation ───────┤
   ↓                │
5. Circuit IR ──────┼→ 6. Type System
   ↓                │      ↓
   └────────────────┼→ 7. Ownership Checker
                    │
8. Lexer ───────────┐
   ↓                │
9. Parser ──────────┼→ 10. Semantic Analyzer
   ↓                │        ↓
   └────────────────┘    11. IR Generator
                             ↓
12. Optimizer ──────────────┐
    ↓                       │
13. Equivalence ────────────┼→ 14. Verifier
    ↓                       │
15. Quipper Importer ───────┤
    ↓                       │
16. Simulator ──────────────┤
    ↓                       │
17. OpenQASM ───────────────┤
    ↓                       │
18. Main Driver ────────────┘
    ↓
19. Fixture (743-wire validation)
    ↓
20. Full Build Verification
```

## Policy Enforcement

### AUTHORIZATION

Every tool call must pass:

```
candidate_action
    ↓
┌─────────────────────────┐
│ GLOBAL_POLICY_FILTER    │
│ ───────────────────────  │
│ • Only build on master  │
│ • No force pushes       │
│ • Verify before commit  │
│ • No external networks  │
└─────────────────────────┘
    ↓
┌─────────────────────────┐
│ TOOL_REGISTRY           │
│ ───────────────────────  │
│ • Tool exists?          │
│ • Parameters valid?     │
│ • Timeout set?          │
│ • Safety class OK?      │
└─────────────────────────┘
    ↓
┌─────────────────────────┐
│ RESOURCE_LOCK           │
│ ───────────────────────  │
│ • Build lock acquired?  │
│ • No conflicts?         │
│ • Exclusive resource?   │
└─────────────────────────┘
    ↓
EXECUTE
```

### PARAMETER VALIDATION

Wire module:

```
wire_count:
  minimum = 1
  maximum = 4096

wire_id:
  minimum = 0
  maximum = 4095

canonical_mapping:
  must be bijective
  must be 743-wire conformant
```

Gate model:

```
gate_type:
  enum = [PAULI_X, PAULI_Y, PAULI_Z, HADAMARD, CNOT, T, S, ...]
  
gate_qubits:
  minimum = 1
  maximum = 3
```

Circuit IR:

```
dependency_graph:
  must be acyclic
  must be topologically sortable
  
operation_count:
  maximum = 65536
```

### DETERMINISTIC POLICY LAYER

```
LLM: "Build the Wire module"
    ↓
PROPOSED ACTION: qcl.build.wire { module: "Wire" }
    ↓
DETERMINISTIC POLICY:
  ✓ Tool exists in registry
  ✓ Parameters conform to schema
  ✓ No build in progress
  ✓ Master branch
  ✓ Resource available
    ↓
EXECUTE: cabal build QCL.IR.Wire
    ↓
OBSERVATION:
  status: SUCCESS
  build_time: 12.3s
  warnings: 0
  errors: 0
```

## State Machine

```
IDLE
 ↓
RECEIVED (User request: "Build quantum-crypto-lang")
 ↓
PLANNING (Generate 18-todo schedule)
 ↓
TODO_1_PENDING (Project structure)
 ├─ AWAITING_POLICY
 ├─ AUTHORIZED
 ├─ EXECUTING (cabal init, setup files)
 ├─ OBSERVING (check success)
 ├─ VALIDATING (can we proceed?)
 └─ COMPLETED
 ↓
TODO_2_PENDING (Wire model)
 ├─ AWAITING_POLICY
 ├─ AUTHORIZED
 ├─ EXECUTING
 ├─ OBSERVING
 ├─ VALIDATING
 └─ COMPLETED
 ↓
... (todos 3-17)
 ↓
TODO_18_PENDING (Full verification)
 ├─ AWAITING_POLICY
 ├─ AUTHORIZED
 ├─ EXECUTING
 ├─ OBSERVING
 ├─ VALIDATING
 └─ COMPLETED
 ↓
COMPLETED
```

## Audit Log

Every todo records:

```
{
  "request_id": "req-20260912-001",
  "todo_id": 2,
  "todo_name": "Wire model",
  "model_decision": "Build Wire.hs with 743-wire support",
  "candidate_action": "qcl.build.wire",
  "policy_evaluation": "AUTHORIZED",
  "authorization": "ALLOWED",
  "tool_invocation": "cabal build QCL.IR.Wire",
  "arguments": {
    "module": "QCL.IR.Wire",
    "target": "lib",
    "verbose": true
  },
  "execution_result": "SUCCESS",
  "execution_time": "12.3s",
  "stdout": "...",
  "stderr": "...",
  "observation": {
    "status": "SUCCESS",
    "warnings": 0,
    "errors": 0
  },
  "verification": "PASSED",
  "state_transition": "COMPLETED",
  "final_outcome": "TODO_2_COMPLETED",
  "timestamp": "2026-09-12T14:23:45Z"
}
```

## Failure Handling

```
Failure Categories:

INVALID_TOOL
  → qcl.build.wire does not exist

INVALID_ARGUMENT
  → wire_count = 99999 (exceeds maximum 4096)

UNAUTHORIZED
  → Attempted to build on non-master branch

SAFETY_VIOLATION
  → Attempted network access from Wire module

RESOURCE_CONFLICT
  → Build already in progress

TIMEOUT
  → cabal build exceeded 60s

BACKEND_FAILURE
  → ghc compiler crashed

VERIFICATION_FAILURE
  → Lean 4 proof of Wire invariants failed

TELEMETRY_FAILURE
  → Unable to record audit log

MODEL_FAILURE
  → LLM refused to propose next action
```

Every failure is logged and blocks progression until resolved.

## Retry Policy

```
READ_ONLY (qcl.verify.wire):
  retry = PERMITTED (up to 3 times)

MUTATING (qcl.build.wire):
  retry = POLICY_REQUIRED
  (Only retry if idempotent, timeout was cause, no side effects)

IRREVERSIBLE (qcl.commit.wire):
  retry = PROHIBITED (manual intervention required)
```

## Multi-Agent Extension

For parallel module building:

```
                   ORCHESTRATOR
                        ↓
        ┌────────────────┼────────────────┐
        ↓                ↓                ↓
   PLANNING            VERIFYING      EXECUTION
     AGENT              AGENT           AGENT
        ↓                ↓                ↓
   "What needs      "Does Wire       "Build Gate"
    building?"      satisfy Lean     "Build Op"
                    invariants?"
```

PLANNING AGENT: Analyzes dependency DAG, proposes next todos.
VERIFYING AGENT: Runs Lean 4 proofs, validates builds.
EXECUTION AGENT: Runs cabal, executes shell commands.

All agents communicate through the orchestrator.

No direct agent-to-agent execution.

## Summary

The ROSA model for quantum-crypto-lang:

- **18 typed tools** (modules to build)
- **1 dependency DAG** (execution order)
- **Policy-enforced execution** (no unsafe builds)
- **Verified state machine** (IDLE → PLANNING → TODO_N → COMPLETED)
- **Immutable audit log** (full reconstruction possible)
- **Deterministic control layer** (LLM proposes, infrastructure decides)
- **Failure categories** (explicit recovery paths)
- **Multi-agent coordination** (parallel verification + execution)
- **Resource locking** (prevent conflicts)

Build is verifiable, auditable, and deterministic.
