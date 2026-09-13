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

-- | IR Generator
--
-- Converts analyzed AST to Quantum Intermediate Representation (IR)
-- producing Circuit IR with:
--   - Resolved wire allocations
--   - Operation DAG
--   - Type annotations
--   - Source traceability

module QCL.Semantic.IRGenerator where

import Data.Aeson
import Data.List (nubBy)
import GHC.Generics
import qualified Data.Map as Map

import QCL.Syntax.Parser
import QCL.Syntax.Lexer
import QCL.Semantic.Analyzer
import QCL.IR.Wire
import QCL.IR.Gate
import QCL.IR.Operation
import QCL.IR.Circuit
import QCL.IR.Register
import QCL.Type.System

-- | IR generation error
data IRGenerationError = IRGenerationError
  { irrMsg :: String
  , irrContext :: String
  } deriving (Show, Eq, Generic)

instance ToJSON IRGenerationError
instance FromJSON IRGenerationError

-- | IR generation result
type IRGenResult a = Either [IRGenerationError] a

-- | Code generation context
data CodeGenContext = CodeGenContext
  { cgCircuit :: Circuit
  , cgRegisterMap :: RegisterMapping
  , cgGateLibrary :: GateLibrary
  , cgTimeStep :: Int
  , cgWireCounter :: Int
  , cgOperationCounter :: Int
  , cgErrors :: [IRGenerationError]
  } deriving (Show, Eq, Generic)

instance ToJSON CodeGenContext
instance FromJSON CodeGenContext

-- | Initialize code generation context
initCodeGenContext :: Int -> CodeGenContext
initCodeGenContext nQubits = CodeGenContext
  { cgCircuit = createCircuit (CircuitName "generated") nQubits
  , cgRegisterMap = createStandardRegisters nQubits 0
  , cgGateLibrary = createStandardGateLibrary
  , cgTimeStep = 0
  , cgWireCounter = nQubits
  , cgOperationCounter = 0
  , cgErrors = []
  }

-- | Generate IR from analyzed program
generateIR :: AnalyzedProgram -> IRGenResult Circuit
generateIR analyzed = do
  -- Extract all register declarations to determine circuit size
  let regDecls = [r | AnalyzedRegisterDecl r <- analyzedDecls analyzed]
  let totalQubits = sum [case regSize r of
                           IntLit n -> n
                           _ -> 0 | r <- regDecls]

  -- Initialize code generation context
  let ctx0 = initCodeGenContext totalQubits

  -- Add all registers to context
  ctx1 <- foldM addRegisterToContext ctx0 regDecls

  -- Generate code for all declarations
  ctx2 <- foldM generateDeclaration ctx1 (analyzedDecls analyzed)

  -- Check for errors
  if not (null (cgErrors ctx2))
    then Left (cgErrors ctx2)
    else Right (cgCircuit ctx2)

-- | Add register to code generation context
addRegisterToContext :: CodeGenContext -> RegisterDecl -> IRGenResult CodeGenContext
addRegisterToContext ctx decl = do
  let size = case regSize decl of
        IntLit n -> Right n
        _ -> Left [IRGenerationError "Register size must be literal" "register"]

  case size of
    Left err -> Left err
    Right n -> do
      -- Allocate wires for register
      let wires = [WireId i | i <- [cgWireCounter ctx .. cgWireCounter ctx + n - 1]]
      let regType = case regType decl of
            QuantumReg -> QCL.IR.Register.QRegType
            AncillaReg -> AncillaRegister
            ClassicalReg -> ClassicalRegister

      let qreg = QuantumRegister
            (RegisterName (regName decl))
            regType
            n
            wires
            Nothing
            Nothing
            Nothing

      -- Add to register mapping
      case addQuantumRegister qreg (cgRegisterMap ctx) of
        Left err -> Left [IRGenerationError err "register mapping"]
        Right newRegMap -> Right $ ctx
          { cgRegisterMap = newRegMap
          , cgWireCounter = cgWireCounter ctx + n
          }

-- | Generate code for declaration
generateDeclaration :: CodeGenContext -> AnalyzedDecl -> IRGenResult CodeGenContext
generateDeclaration ctx decl = case decl of
  AnalyzedRegisterDecl _ -> Right ctx  -- Already processed

  AnalyzedGateDecl gateSpec -> do
    -- Generate operations from gate body
    ctx' <- foldM generateStatement ctx (gateBody gateSpec)
    Right ctx'

  AnalyzedCircuitDecl circDecl -> do
    -- Generate circuit operations
    ctx' <- foldM generateStatement ctx (circuitBody circDecl)
    Right ctx'

  AnalyzedFunctionDecl _ -> Right ctx  -- Classical function, skip

-- | Generate IR for statement
generateStatement :: CodeGenContext -> Statement -> IRGenResult CodeGenContext
generateStatement ctx stmt = case stmt of
  GateCall gateName exprs -> do
    -- Look up gate in library
    let gid = GateId gateName
    case Map.lookup gid (cgGateLibrary ctx) of
      Nothing -> Left [IRGenerationError ("Gate not found: " ++ gateName) "gate call"]
      Just gateObj -> do
        -- Create operation from gate
        let opid = OperationId ("op_" ++ show (cgOperationCounter ctx))
        let operation = createGateOperation opid gateObj [] [] (cgTimeStep ctx)

        -- Add to circuit
        case addOperationToCircuit operation (cgCircuit ctx) of
          Left err -> Left [IRGenerationError err "operation"]
          Right newCirc -> Right $ ctx
            { cgCircuit = newCirc
            , cgTimeStep = cgTimeStep ctx + 1
            , cgOperationCounter = cgOperationCounter ctx + 1
            }

  Measurement expr basis -> do
    -- Create measurement operation
    let opid = OperationId ("meas_" ++ show (cgOperationCounter ctx))
    let operation = createMeasurementOperation opid [] (cgTimeStep ctx)

    case addOperationToCircuit operation (cgCircuit ctx) of
      Left err -> Left [IRGenerationError err "measurement"]
      Right newCirc -> Right $ ctx
        { cgCircuit = newCirc
        , cgTimeStep = cgTimeStep ctx + 1
        , cgOperationCounter = cgOperationCounter ctx + 1
        }

  Reset expr -> do
    let opid = OperationId ("reset_" ++ show (cgOperationCounter ctx))
    let operation = createResetOperation opid [] (cgTimeStep ctx)

    case addOperationToCircuit operation (cgCircuit ctx) of
      Left err -> Left [IRGenerationError err "reset"]
      Right newCirc -> Right $ ctx
        { cgCircuit = newCirc
        , cgTimeStep = cgTimeStep ctx + 1
        , cgOperationCounter = cgOperationCounter ctx + 1
        }

  Barrier -> do
    let opid = OperationId ("barrier_" ++ show (cgOperationCounter ctx))
    let operation = createBarrierOperation opid [] (cgTimeStep ctx)

    case addOperationToCircuit operation (cgCircuit ctx) of
      Left err -> Left [IRGenerationError err "barrier"]
      Right newCirc -> Right $ ctx
        { cgCircuit = newCirc
        , cgTimeStep = cgTimeStep ctx + 1
        , cgOperationCounter = cgOperationCounter ctx + 1
        }

  Block stmts -> foldM generateStatement ctx stmts

  _ -> Right ctx  -- Skip other statement types for now

-- | Generate IR from source code
generateIRFromSource :: String -> IRGenResult Circuit
generateIRFromSource source = do
  -- Tokenize
  tokens <- case tokenize "input.qcl" source of
    Left err -> Left [IRGenerationError err "tokenization"]
    Right toks -> Right toks

  -- Parse
  prog <- case parseProgram tokens of
    Left parseErr -> Left [IRGenerationError (errorMsg parseErr) "parsing"]
    Right p -> Right p

  -- Analyze
  analyzed <- case analyzeProgram prog of
    Left semErrs -> Left [IRGenerationError (prettySemErrors semErrs) "semantic analysis"]
    Right a -> Right a

  -- Generate IR
  generateIR analyzed

-- | IR generation report
data IRReport = IRReport
  { reportCircuit :: Maybe Circuit
  , reportErrors :: [IRGenerationError]
  , reportSuccess :: Bool
  , reportStats :: Maybe CircuitStats
  } deriving (Show, Eq, Generic)

instance ToJSON IRReport
instance FromJSON IRReport

-- | Generate IR with reporting
generateIRWithReport :: String -> IRReport
generateIRWithReport source =
  case generateIRFromSource source of
    Left errs -> IRReport Nothing errs False Nothing
    Right circ -> case getCircuitStats circ of
      Left _ -> IRReport (Just circ) [] True Nothing
      Right stats -> IRReport (Just circ) [] True (Just stats)

-- | Pretty-print IR generation error
prettyIRError :: IRGenerationError -> String
prettyIRError err =
  let ctxStr = if null (irrContext err) then "" else " (" ++ irrContext err ++ ")"
  in "IR Error: " ++ irrMsg err ++ ctxStr

-- | Pretty-print all IR errors
prettyIRErrors :: [IRGenerationError] -> String
prettyIRErrors errs = unlines (map prettyIRError errs)

-- | Helper: fold with error handling
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a [] = return a
foldM f a (b:bs) = do
  a' <- f a b
  foldM f a' bs

-- | Extract circuit body (for declarations)
circuitBody :: CircuitDecl -> [Statement]
circuitBody (CircuitDecl _ _ body) = body

-- | Trace IR generation (for debugging)
data IRTrace = IRTrace
  { traceStep :: Int
  , traceOperation :: String
  , traceTime :: Int
  , traceWires :: [Int]
  } deriving (Show, Eq, Generic)

instance ToJSON IRTrace
instance FromJSON IRTrace

-- | Collect IR generation trace
traceIRGeneration :: CodeGenContext -> [IRTrace]
traceIRGeneration ctx =
  let ops = getAllOperations (circuitDAG (cgCircuit ctx))
  in zipWith (\i op -> IRTrace i (show (opType op)) (timestamp op) [])
       [0..] ops
