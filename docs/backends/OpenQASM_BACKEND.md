# OpenQASM 2.0 Backend for quantum-crypto-lang

## Overview

The `QCL.Backend.OpenQASM` module provides a complete implementation of an OpenQASM 2.0 exporter for the quantum-crypto-lang compiler. This backend converts QCL circuits to OpenQASM 2.0 standard format, enabling compatibility with major quantum computing platforms.

**Module Location:** `src/QCL/Backend/OpenQASM.hs`

## Features

- **OpenQASM 2.0 Compliance:** Generates valid OpenQASM 2.0 programs compatible with IBM Qiskit, Cirq, and other platforms
- **Register Management:** Handles quantum (qreg) and classical (creg) register declarations
- **Complete Gate Support:** Maps QCL operations to OpenQASM instructions
  - Unary gates: I, X, Y, Z, H, S, S†, T, T†, √X, √Y, √Z
  - Parametric gates: Rx(θ), Ry(θ), Rz(θ), Phase(θ)
  - Binary gates: CNOT, CY, CZ, SWAP, iSWAP, XX(θ), YY(θ), ZZ(θ)
  - Ternary gates: Toffoli (CCX), Controlled-SWAP
- **Measurements:** Conversion of measurement operations to measure instructions
- **Resets:** Support for qubit reset operations
- **Barriers:** Global synchronization via barrier instructions
- **Validation:** Built-in OpenQASM format validation

## Type Definitions

### OpenQASMProgram

```haskell
data OpenQASMProgram = OpenQASMProgram
  { qasmVersion :: String              -- "OPENQASM 2.0"
  , qasmIncludes :: [String]           -- Include statements
  , qasmRegisters :: [String]          -- Register declarations
  , qasmOperations :: [String]         -- Gate operations
  , qasmComments :: [String]           -- Metadata comments
  } deriving (Show, Eq, Generic)
```

## Core Functions

### exportCircuit :: Circuit -> String

Converts a complete QCL circuit to a valid OpenQASM 2.0 program string.

**Example:**
```haskell
let circ = createCircuit (CircuitName "bell") 2
let qasmCode = exportCircuit circ
putStrLn qasmCode
```

**Output:**
```
OPENQASM 2.0;
include "qelib1.inc";
qreg q[2];
creg c[2];
```

### registerDeclarations :: Circuit -> [String]

Generates OpenQASM register declarations from circuit structure.

**Returns:**
- Quantum register declarations (qreg)
- Classical register declarations (creg)

### operationsToQASM :: Circuit -> [String]

Converts all circuit operations to OpenQASM instructions in topological order.

### operationToQASM :: Circuit -> Operation -> [String]

Converts a single operation to one or more OpenQASM instructions.

**Handles:**
- GateOperation → gate instruction
- MeasurementOperation → measure instruction
- ResetOperation → reset instruction
- BarrierOperation → barrier instruction

### gateToQASM :: Gate -> [WireId] -> [String]

Maps QCL gates to OpenQASM instructions.

### validateOpenQASM :: String -> Either String ()

Validates OpenQASM 2.0 format and returns:
- `Right ()` if valid
- `Left error_message` if invalid

## Gate Mapping Reference

### Unary Gates

| QCL Gate | OpenQASM | Example |
|----------|----------|---------|
| I        | id       | id q[0]; |
| X        | x        | x q[0]; |
| Y        | y        | y q[0]; |
| Z        | z        | z q[0]; |
| H        | h        | h q[0]; |
| S        | s        | s q[0]; |
| S†       | sdg      | sdg q[0]; |
| T        | t        | t q[0]; |
| T†       | tdg      | tdg q[0]; |
| √X       | sx       | sx q[0]; |
| √X†      | sxdg     | sxdg q[0]; |

### Parametric Gates

| QCL Gate   | OpenQASM | Example |
|-----------|----------|---------|
| Rx(θ)     | rx       | rx(1.571) q[0]; |
| Ry(θ)     | ry       | ry(3.142) q[0]; |
| Rz(θ)     | rz       | rz(1.571) q[0]; |
| Phase(θ)  | p        | p(0.785) q[0]; |

### Two-Qubit Gates

| QCL Gate | OpenQASM | Example |
|----------|----------|---------|
| CNOT     | cx       | cx q[0],q[1]; |
| CY       | cy       | cy q[0],q[1]; |
| CZ       | cz       | cz q[0],q[1]; |
| SWAP     | swap     | swap q[0],q[1]; |
| iSWAP    | iswap    | iswap q[0],q[1]; |
| XX(θ)    | rxx      | rxx(1.571) q[0],q[1]; |
| YY(θ)    | ryy      | ryy(1.571) q[0],q[1]; |
| ZZ(θ)    | rzz      | rzz(1.571) q[0],q[1]; |

### Three-Qubit Gates

| QCL Gate | OpenQASM | Example |
|----------|----------|---------|
| Toffoli  | ccx      | ccx q[0],q[1],q[2]; |
| CSwap    | cswap    | cswap q[0],q[1],q[2]; |

### Special Operations

| Operation | OpenQASM |
|-----------|----------|
| Measurement | measure q[i] -> c[j]; |
| Reset       | reset q[i]; |
| Barrier     | barrier q; |

## Example Usage

### Basic Circuit Export

```haskell
import QCL.IR.Circuit
import QCL.Backend.OpenQASM

main :: IO ()
main = do
  -- Create a simple 2-qubit circuit
  let circ = createCircuit (CircuitName "bell_pair") 2

  -- Add operations...
  -- (circuit building code)

  -- Export to OpenQASM
  let qasmCode = exportCircuit circ
  putStrLn qasmCode
```

### With Metadata

```haskell
-- Export with circuit information
let progWithMeta = exportCircuitWithMetadata circ
putStrLn $ unlines (qasmComments progWithMeta)
putStrLn (qasmVersion progWithMeta)
```

### Validation

```haskell
-- Validate OpenQASM program
case validateOpenQASM qasmCode of
  Right () -> putStrLn "Valid OpenQASM!"
  Left err -> putStrLn $ "Error: " ++ err
```

## Register Sizing

The backend automatically determines register sizes from circuit structure:

- **Quantum register:** Size equals total number of qubits in circuit
- **Classical register:** Size equals number of measurements (or qubits if measurements = 0)

**Example for 743-qubit circuit:**
```
qreg q[743];
creg c[700];
```

## Output Format Characteristics

- **Header:** Always includes `OPENQASM 2.0;` and `include "qelib1.inc";`
- **Whitespace:** Properly formatted with single semicolons
- **Comments:** Include circuit metadata (optional)
- **Ordering:** Operations ordered by timestamp (topological sort)
- **Precision:** Floating-point angles formatted to 6 significant digits

## Example Output

```openqasm
OPENQASM 2.0;
include "qelib1.inc";
qreg q[743];
creg c[700];

h q[0];
cx q[0],q[1];
ry(1.5708) q[2];
measure q[0] -> c[0];
barrier q;
reset q[1];
```

## Integration with Quantum Platforms

The generated OpenQASM code is compatible with:

- **IBM Qiskit:** Native support via `QuantumCircuit.from_qasm_str()`
- **Google Cirq:** Import via OpenQASM support
- **IonQ:** OpenQASM submission format
- **Rigetti:** Quil -> OpenQASM conversion available
- **Quantinuum:** OpenQASM format supported

## Validation Features

The `validateOpenQASM` function checks:

1. **Header presence:** Must start with `OPENQASM 2.0;`
2. **Register syntax:** Valid qreg/creg declarations
3. **Register names:** Must match pattern `name[size]`
4. **Size values:** Must be positive integers

## Error Handling

All functions return `Either String a` for error cases:

```haskell
validateOpenQASM :: String -> Either String ()
-- Returns Left with descriptive error message on failure
```

## Performance Considerations

- **Linear complexity:** O(n) where n = number of operations
- **Memory efficiency:** Streaming construction of output string
- **Topological ordering:** DAG traversal ensures dependency-respecting output

## Future Extensions

- [ ] Custom gate definitions (gate ... { ... })
- [ ] Qubit coupling maps
- [ ] Noise model specifications
- [ ] OpenQASM 3.0 support
- [ ] Reverse compilation (OpenQASM → QCL)
- [ ] Optimization passes (commutation, fusion)

## Testing

Run the test suite:

```bash
cd quantum-crypto-lang
cabal test
```

OpenQASM-specific tests in `test/Tests/OpenQASM.hs`

## References

- [OpenQASM 2.0 Specification](https://github.com/Qiskit/openqasm)
- [IBM Qiskit Documentation](https://qiskit.org)
- [Quantum Instruction Set Alliance](https://qiskit.org/open-qasm/)
