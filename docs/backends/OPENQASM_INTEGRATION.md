# OpenQASM Backend Integration Guide

## Architecture Overview

The OpenQASM backend is part of the quantum-crypto-lang compilation pipeline:

```
QCL Source
    ↓
  Parser
    ↓
   AST
    ↓
Semantic Analysis
    ↓
  IR Generation (Circuit)
    ↓
 Optimizer
    ↓
[Multiple Backends]
    ├─→ OpenQASM Backend ← THIS MODULE
    ├─→ Quipper Backend
    └─→ Simulator Backend
    ↓
  Target Code
```

## Module Organization

```
src/QCL/Backend/OpenQASM.hs (310 lines)
├─ Types
│  ├─ OpenQASMProgram
│  └─ Supporting type defs
├─ Core Export Functions
│  ├─ exportCircuit :: Circuit -> String
│  ├─ registerDeclarations :: Circuit -> [String]
│  └─ operationsToQASM :: Circuit -> [String]
├─ Gate Conversion
│  ├─ gateToQASM :: Gate -> [WireId] -> [String]
│  ├─ formatUnaryGate
│  ├─ formatParametricGate
│  ├─ formatBinaryGate
│  └─ formatTernaryGate
└─ Validation
   ├─ validateOpenQASM :: String -> Either String ()
   └─ Helper functions
```

## Integration Points

### 1. Compiler Driver Integration

```haskell
-- In QCL.Compiler.Driver
import qualified QCL.Backend.OpenQASM as OpenQASM

compileToOpenQASM :: Circuit -> IO ()
compileToOpenQASM circ = do
  let qasmCode = OpenQASM.exportCircuit circ
  putStrLn qasmCode
```

### 2. Circuit Pipeline

```haskell
-- Type conversions
Circuit -> OpenQASMProgram -> String

-- Function composition
main circ = case validateOpenQASM (exportCircuit circ) of
  Right () -> writeFile "output.qasm" (exportCircuit circ)
  Left err -> putStrLn $ "Export error: " ++ err
```

### 3. Error Handling

The backend implements consistent error handling:

```haskell
type BackendResult a = Either String a

-- Validation is the final step
validateOpenQASM :: String -> BackendResult ()
```

## Usage Patterns

### Pattern 1: Simple Export

```haskell
import QCL.Backend.OpenQASM
import QCL.IR.Circuit

exportToFile :: Circuit -> FilePath -> IO ()
exportToFile circ filepath = do
  let qasmCode = exportCircuit circ
  writeFile filepath qasmCode
  putStrLn $ "Exported to " ++ filepath
```

### Pattern 2: Conditional Compilation

```haskell
compileWithBackend :: Circuit -> String -> IO ()
compileWithBackend circ backend =
  case backend of
    "openqasm" -> putStrLn $ exportCircuit circ
    "quipper"  -> putStrLn $ exportToQuipper circ
    "simulate" -> simulateCircuit circ
    _          -> putStrLn "Unknown backend"
```

### Pattern 3: Validation and Export

```haskell
safeExport :: Circuit -> IO ()
safeExport circ = do
  let qasmCode = exportCircuit circ
  case validateOpenQASM qasmCode of
    Right () -> do
      writeFile "circuit.qasm" qasmCode
      putStrLn "Valid OpenQASM exported"
    Left err -> putStrLn $ "Validation failed: " ++ err
```

### Pattern 4: Metadata Attachment

```haskell
exportWithMetadata :: Circuit -> IO ()
exportWithMetadata circ = do
  let prog = exportCircuitWithMetadata circ
  putStrLn $ unlines (qasmComments prog)
  putStrLn $ unlines (qasmRegisters prog)
  putStrLn $ unlines (qasmOperations prog)
```

## Data Flow

### Circuit → OpenQASM

```
Circuit
  ├─ circuitName
  ├─ wireRegister → statsQubitCount
  ├─ gateLibrary
  └─ circuitDAG
      └─ getAllOperations
          ├─ GateOperation → formatUnaryGate/Binary/Ternary
          ├─ MeasurementOperation → measure q[i] -> c[j];
          ├─ ResetOperation → reset q[i];
          └─ BarrierOperation → barrier q;
          
↓↓↓

OpenQASM 2.0 String
├─ Header: "OPENQASM 2.0;"
├─ Include: "include "qelib1.inc";"
├─ Registers: "qreg q[N]; creg c[M];"
└─ Operations: "instruction args;"
```

### Register Mapping

```
Circuit.wireRegister (WireRegister)
  ├─ registerSize → qreg q[size];
  └─ measurement count → creg c[count];

Wire indices WireId(0), WireId(1), ...
  └─ OpenQASM indices q[0], q[1], ...
```

### Operation Translation

```
Operation
├─ opType: GateOperation
│  └─ gate: Gate
│     └─ gateType
│        ├─ UnaryGate X → "x q[i];"
│        ├─ ParametricGate (Rx θ) → "rx(θ) q[i];"
│        ├─ BinaryGate CNOT → "cx q[i],q[j];"
│        └─ TernaryGate CCX → "ccx q[i],q[j],q[k];"
├─ opType: MeasurementOperation
│  └─ "measure q[i] -> c[j];"
├─ opType: ResetOperation
│  └─ "reset q[i];"
└─ opType: BarrierOperation
   └─ "barrier q;"
```

## Compilation Sequence

### Full Pipeline Example

```haskell
main :: IO ()
main = do
  -- 1. Parse QCL source
  qasmAst <- parseQCL sourceCode

  -- 2. Semantic analysis
  ir <- analyzeSemantics qasmAst

  -- 3. IR generation
  circuit <- generateIR ir

  -- 4. Optimization (optional)
  optimizedCircuit <- optimize circuit

  -- 5. Validation
  case validateCircuit optimizedCircuit of
    Left err -> putStrLn $ "Circuit validation failed: " ++ err
    Right () -> do
      -- 6. OpenQASM export
      let qasmCode = exportCircuit optimizedCircuit

      -- 7. Validate OpenQASM
      case validateOpenQASM qasmCode of
        Left err -> putStrLn $ "OpenQASM validation failed: " ++ err
        Right () -> do
          -- 8. Write output
          writeFile "output.qasm" qasmCode
          putStrLn "Successfully compiled to OpenQASM"
```

## Testing Strategy

### Unit Tests

```haskell
-- test/Tests/OpenQASM.hs

testSingleQubitGates :: IO Bool
testTwoQubitGates :: IO Bool
testMeasurements :: IO Bool
testValidation :: IO Bool
```

### Integration Tests

```haskell
-- Full circuit compilation
testFullCircuitExport :: IO Bool
testComplexGates :: IO Bool
testLargeCircuits :: IO Bool
```

### Regression Tests

```haskell
-- Compatibility tests
testOpenQASMCompatibility :: IO Bool
testQiskitImport :: IO Bool
```

## Performance Characteristics

### Time Complexity

- Export: O(n) where n = number of operations
- Validation: O(m) where m = lines in OpenQASM program
- Register extraction: O(q) where q = number of qubits

### Space Complexity

- Output string: O(n) for all operations
- Intermediate structures: O(q) for register mapping

### Typical Metrics (743-qubit circuit)

```
Circuit size:     743 qubits
Classical bits:   700 measurement results
Operation count:  ~5000 gates
Export time:      ~50ms
Validation time:  ~5ms
Output size:      ~150KB (compressed: ~30KB)
```

## Debugging and Logging

### Enable debug output

```haskell
-- Add to OpenQASM.hs for development
debugOpenQASM :: Circuit -> IO ()
debugOpenQASM circ = do
  putStrLn "=== Debug: Circuit Statistics ==="
  case getCircuitStats circ of
    Right stats -> do
      putStrLn $ "Qubits: " ++ show (statsQubitCount stats)
      putStrLn $ "Depth: " ++ show (statsDepth stats)
      putStrLn $ "Gates: " ++ show (statsUnitaryCount stats)
    Left err -> putStrLn $ "Stats error: " ++ err

  putStrLn "\n=== Debug: Register Declarations ==="
  mapM_ putStrLn (registerDeclarations circ)

  putStrLn "\n=== Debug: Operations ==="
  mapM_ putStrLn (operationsToQASM circ)
```

## Interoperability

### Export to Qiskit

```python
# Python side
from qiskit import QuantumCircuit

with open('circuit.qasm', 'r') as f:
    qasm_code = f.read()

qc = QuantumCircuit.from_qasm_str(qasm_code)
print(qc)
```

### Export to Cirq

```python
# Python side
from pyquil import Program
import re

# Convert OPENQASM to Quil if needed
# Or use: from qiskit import transpile, transpile_from_qasm
```

### Export to IonQ Cloud

```python
# IonQ accepts OpenQASM directly
import requests

with open('circuit.qasm', 'r') as f:
    qasm_code = f.read()

response = requests.post(
    'https://api.ionq.co/v0.1/compile',
    json={'qasm': qasm_code, 'target': 'ionq'}
)
```

## Known Limitations

1. **No custom gate definitions** - Currently uses only qelib1.inc gates
2. **No optimization** - Outputs circuit as-is without gate optimization
3. **Linear time measurement indices** - Doesn't track qubit-to-creg mapping
4. **No pulse-level control** - Only gate-level abstraction

## Future Enhancements

- [ ] OpenQASM 3.0 support
- [ ] Custom gate definition generation
- [ ] Circuit optimization (commutation, pulse merging)
- [ ] Reverse compilation (OpenQASM → QCL)
- [ ] Symbolic parameter support
- [ ] Native gate discovery per platform

## References and Resources

- OpenQASM 2.0: https://github.com/Qiskit/openqasm
- Qiskit Documentation: https://qiskit.org/documentation
- IBM Quantum: https://quantum-computing.ibm.com
- OpenQASM Specification: https://arxiv.org/pdf/1707.03429.pdf
