# WORM-JOVIAL — Quantum Cryptographic Language

[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](LICENSE)
[![Haskell](https://img.shields.io/badge/Haskell-GHC%209.x-purple.svg)](haskell/)
[![JOVIAL](https://img.shields.io/badge/JOVIAL-J73%20600%2B%20LOC-orange.svg)](jovial/WORMJOV.jov)
[![Quipper](https://img.shields.io/badge/backend-Quipper%20743--wire-green.svg)](docs/backends/QUIPPER_IMPORTER.md)
[![OpenQASM](https://img.shields.io/badge/backend-OpenQASM%202.0-lightblue.svg)](docs/backends/OpenQASM_BACKEND.md)
[![Lines](https://img.shields.io/badge/Haskell-7%2C118%20lines-brightgreen.svg)](haskell/src/QCL/)
[![ROSA](https://img.shields.io/badge/orchestration-ROSA-yellow.svg)](docs/architecture/ROSA_ORCHESTRATOR.md)

---

Write-Once / Read-Once cryptographic semantics implemented across two paradigms:

| Layer | Language | Purpose |
|---|---|---|
| [`jovial/`](jovial/) | JOVIAL J73 | Bare-metal WORM-SECRET — static allocation, radiation-hardened, 20 demos |
| [`haskell/`](haskell/) | Haskell + Quipper | Quantum Crypto Language — 743-wire canonical compiler, ROSA orchestration |

Both enforce the same invariant: **write once, consume the secrets, seal, allow exactly one recovery.**

---

## Repository Layout

```
worm-jovial/
│
├── jovial/                        JOVIAL J73 implementation
│   └── WORMJOV.jov                600+ line WORM-SECRET engine
│
├── haskell/                       Haskell QCL compiler
│   ├── quantum-crypto-lang.cabal  Project definition
│   ├── src/QCL/
│   │   ├── IR/                    Wire · Gate · Operation · Circuit · Register
│   │   ├── Type/                  System · Ownership (linear types)
│   │   ├── Syntax/                Lexer · Parser · AST
│   │   ├── Semantic/              Analyzer · IRGenerator
│   │   ├── Optimization/          Optimizer
│   │   ├── Verification/          Equivalence · Verifier
│   │   ├── Backend/               Quipper · Simulator · OpenQASM
│   │   ├── Compiler/              Driver · Fixture · Main
│   │   ├── Crypto/                TodoLibrary (18-todo state machine)
│   │   └── Daemon/                AsyncExecutor (ROSA downstream)
│   └── test/
│       └── Tests/                 Wire · Gate · Parser · Fixture · OpenQASM · Quipper
│
├── docs/
│   ├── architecture/              ROSA Orchestrator · Pipeline Verification
│   ├── backends/                  Quipper Importer · OpenQASM Backend
│   └── integration/               Integration Guide · Implementation Summary
│
└── LICENSE                        GNU Affero GPL v3.0
```

---

## JOVIAL Layer

`jovial/WORMJOV.jov` is a dense, hand-rolled 600+ line WORM engine in classic JOVIAL J73/J3.

```
EMPTY → WRITEP → SUPKEY → ENCRYPT (plain+key zeroed) → SEALED → DECRYPT → CONSUMED
```

**20 demos** cover every edge case: double-write rejection, no-key rejection, double-decrypt rejection, labelled sealed objects, multi-round stress, parallel independent WORMs, CAN-* predicates, post-destroy safety, and 3 self-tests.

**Why JOVIAL?** No `malloc`. Table sizes defined at compile time. Once the WORM_COLLAPSE bit is set, the vault cannot be re-entered. The psychological equivalent of a flight data recorder — bounded, bit-packed, radiation-hardened against recursive re-reads.

Build requires a JOVIAL J73 or J3 compiler. The source is also readable as a formal specification.

---

## Haskell QCL Layer

A full quantum cryptographic programming language with a 7-stage compilation pipeline.

### Architecture

```
┌──────────────────────────────────────┐
│  ROSA Orchestration Layer            │  Planning · tool selection · dep graph
└──────────────┬───────────────────────┘
               │ ExecutionRequest
               ▼
┌──────────────────────────────────────┐
│  Async Execution Daemon              │  Policy · resource control · execution
└──────────────┬───────────────────────┘
               │ ExecutionResponse
               ▼
┌──────────────────────────────────────┐
│  Quantum Compiler Backend            │  Cabal · verification · backends
└──────────────────────────────────────┘
```

### Pipeline

```
Source → Lexer → Parser → Semantic Analyzer → IR Generator
       → Optimizer → Verifier → Backend (Quipper | Simulator | OpenQASM)
```

### Key modules

| Module | Status | Description |
|---|---|---|
| `IR.Wire` | ✓ | 743-wire canonical model, no-cloning enforced |
| `IR.Gate` | ✓ | Canonical gate vocabulary (H, X, Y, Z, S, T, CNOT, CCX, SWAP, RZ…) |
| `Backend.Quipper` | ✓ | Imports wire_0001–wire_0743, maps Quipper gates to QCL |
| `Backend.OpenQASM` | ✓ | Emits OpenQASM 2.0 from QCL circuit IR |
| `Crypto.TodoLibrary` | ✓ | 18-todo ROSA state machine with audit log |
| `Daemon.AsyncExecutor` | ✓ | Typed ExecutionRequest/Response, policy enforcement |
| `Compiler.Driver` | ✓ | Full pipeline: `compile`, `compileToCircuit`, `compileAndVerify` |

### Build

```bash
cd haskell
cabal update
cabal build
cabal test
```

### CLI

```bash
cabal run qcl-compiler -- build quantum-crypto-lang
cabal run qcl-compiler -- verify --fixture 743-wire
cabal run qcl-compiler -- status
```

---

## ROSA Dependency DAG

```
Wire → Gate → Operation → Circuit → Type System → Ownership
Lexer → Parser → Semantic → IR Generator
Optimizer → Equivalence → Verifier
Quipper → Simulator → OpenQASM → Driver → Fixture → Verify All
```

Each node is a typed tool with input/output schema, safety class, execution mode, timeout, and determinism contract. Every build generates an immutable audit record.

---

## License

GNU Affero General Public License v3.0 or later.  
Every source file carries a SPDX-License-Identifier header.  
See [LICENSE](LICENSE) — `SPDX-License-Identifier: AGPL-3.0-or-later`

Copyright (C) 2026 SnapKittyWest. Ahmad Ali Parr, Bel Esprit D'Accord Irrevocable Trust.

---

## Docs

| Document | Description |
|---|---|
| [ROSA Orchestrator](docs/architecture/ROSA_ORCHESTRATOR.md) | 18-tool orchestration model, policy loop, state machine |
| [Pipeline Verification](docs/architecture/PIPELINE_VERIFICATION.md) | End-to-end pipeline proof obligations |
| [Quipper Importer](docs/backends/QUIPPER_IMPORTER.md) | 743-wire extraction, source map, gate mapping |
| [OpenQASM Backend](docs/backends/OpenQASM_BACKEND.md) | Emission, gate translation, circuit serialization |
| [Integration Guide](docs/integration/INTEGRATION_GUIDE.md) | Wiring ROSA + Daemon + Compiler |
| [Implementation Summary](docs/integration/IMPLEMENTATION_SUMMARY.md) | Phase-by-phase delivery summary |
