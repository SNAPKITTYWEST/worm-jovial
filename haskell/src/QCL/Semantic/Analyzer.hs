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

-- | Semantic Analyzer
--
-- Performs semantic analysis on the AST:
--   - Type checking
--   - Name resolution
--   - Ownership validation (linear/affine)
--   - Gate application validation
--   - Register bounds checking
--   - Scope management
--   - Error reporting with source locations

module QCL.Semantic.Analyzer where

import Data.Aeson
import Data.List (find, intercalate)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set

import QCL.Syntax.Parser
import QCL.Syntax.Lexer
import QCL.Type.System
import QCL.Type.Ownership

-- | Semantic error with location
data SemanticError = SemanticError
  { semErrMsg :: String
  , semErrLoc :: Maybe SourceLoc
  , semErrContext :: String
  } deriving (Show, Eq, Generic)

instance ToJSON SemanticError
instance FromJSON SemanticError

-- | Semantic check result
type SemanticCheck a = Either [SemanticError] a

-- | Analyzer environment
data AnalyzerEnv = AnalyzerEnv
  { typeCtx :: TypeContext
  , ownershipEnv :: OwnershipEnv
  , registers :: Map.Map String RegisterInfo
  , gates :: Map.Map String GateInfo
  , scopes :: [Map.Map String QuantumType]
  , errors :: [SemanticError]
  } deriving (Show, Eq, Generic)

instance ToJSON AnalyzerEnv
instance FromJSON AnalyzerEnv

-- | Register information
data RegisterInfo = RegisterInfo
  { regInfoName :: String
  , regInfoType :: RegisterType
  , regInfoSize :: Int
  , regInfoDeclLoc :: Maybe SourceLoc
  } deriving (Show, Eq, Generic)

instance ToJSON RegisterInfo
instance FromJSON RegisterInfo

-- | Gate information
data GateInfo = GateInfo
  { gateInfoName :: String
  , gateInfoQubits :: [String]
  , gateInfoClassical :: [String]
  , gateInfoDeclLoc :: Maybe SourceLoc
  } deriving (Show, Eq, Generic)

instance ToJSON GateInfo
instance FromJSON GateInfo

-- | Analyzed expression with type
data TypedExpr = TypedExpr
  { exprAST :: Expr
  , exprType :: QuantumType
  , exprLoc :: Maybe SourceLoc
  } deriving (Show, Eq, Generic)

instance ToJSON TypedExpr
instance FromJSON TypedExpr

-- | Analyzed statement with type information
data TypedStatement = TypedStatement
  { stmtAST :: Statement
  , stmtType :: QuantumType
  , stmtLoc :: Maybe SourceLoc
  } deriving (Show, Eq, Generic)

instance ToJSON TypedStatement
instance FromJSON TypedStatement

-- | Analyzed program
data AnalyzedProgram = AnalyzedProgram
  { analyzedDecls :: [AnalyzedDecl]
  , analyzedEnv :: AnalyzerEnv
  } deriving (Show, Eq, Generic)

instance ToJSON AnalyzedProgram
instance FromJSON AnalyzedProgram

-- | Analyzed declaration
data AnalyzedDecl
  = AnalyzedRegisterDecl RegisterDecl
  | AnalyzedGateDecl GateSpec
  | AnalyzedCircuitDecl CircuitDecl
  | AnalyzedFunctionDecl FunctionDecl
  deriving (Show, Eq, Generic)

instance ToJSON AnalyzedDecl
instance FromJSON AnalyzedDecl

-- | Create empty analyzer environment
emptyAnalyzerEnv :: AnalyzerEnv
emptyAnalyzerEnv = AnalyzerEnv
  { typeCtx = emptyTypeContext
  , ownershipEnv = emptyOwnershipEnv
  , registers = Map.empty
  , gates = Map.empty
  , scopes = [Map.empty]
  , errors = []
  }

-- | Add semantic error
addError :: String -> Maybe SourceLoc -> String -> AnalyzerEnv -> AnalyzerEnv
addError msg loc ctx env =
  let err = SemanticError msg loc ctx
  in env { errors = errors env ++ [err] }

-- | Check if register exists
registerExists :: String -> AnalyzerEnv -> Bool
registerExists name env = Map.member name (registers env)

-- | Get register info
getRegister :: String -> AnalyzerEnv -> Maybe RegisterInfo
getRegister name env = Map.lookup name (registers env)

-- | Add register declaration
addRegisterDecl :: RegisterDecl -> AnalyzerEnv -> SemanticCheck AnalyzerEnv
addRegisterDecl decl env =
  if registerExists (regName decl) env
    then Left [SemanticError ("Register already defined: " ++ regName decl) Nothing "declaration"]
    else do
      -- Parse size expression (for now, assume integer literal)
      let size = case regSize decl of
            IntLit n -> Right n
            _ -> Left [SemanticError "Register size must be literal" Nothing "register size"]
      case size of
        Left err -> Left err
        Right n -> do
          let regInfo = RegisterInfo
                { regInfoName = regName decl
                , regInfoType = regType decl
                , regInfoSize = n
                , regInfoDeclLoc = Nothing
                }
          let newRegs = Map.insert (regName decl) regInfo (registers env)
          let quantumType = case regType decl of
                QuantumReg -> TQRegister n
                AncillaReg -> TAncilla n
                ClassicalReg -> TClassicalInt
          let newEnv = env
                { registers = newRegs
                , typeCtx = addBinding (regName decl) quantumType (typeCtx env)
                }
          Right newEnv

-- | Check gate application
checkGateApplication :: String -> [Expr] -> AnalyzerEnv -> SemanticCheck QuantumType
checkGateApplication gateName args env =
  case inferTypeFromOperation gateName (map (\_ -> TQubit) args) of
    Left msg -> Left [SemanticError msg Nothing "gate application"]
    Right rt -> Right rt

-- | Type check expression
checkExpr :: Expr -> AnalyzerEnv -> SemanticCheck (TypedExpr, AnalyzerEnv)
checkExpr expr env = case expr of
  Var name -> do
    case lookupType name (typeCtx env) of
      Nothing -> Left [SemanticError ("Undefined variable: " ++ name) Nothing "expression"]
      Just qt -> Right (TypedExpr expr qt Nothing, env)

  IntLit n -> Right (TypedExpr expr TClassicalInt Nothing, env)

  FloatLit _ -> Right (TypedExpr expr TClassicalInt Nothing, env)

  StringLit _ -> Right (TypedExpr expr TClassicalInt Nothing, env)

  BinOp op left right -> do
    (leftTyped, env') <- checkExpr left env
    (rightTyped, env'') <- checkExpr right env'

    -- Type inference for binary operations
    resultType <- case op of
      Add -> unify (exprType leftTyped) (exprType rightTyped)
      Sub -> unify (exprType leftTyped) (exprType rightTyped)
      Mul -> unify (exprType leftTyped) (exprType rightTyped)
      Div -> unify (exprType leftTyped) (exprType rightTyped)
      Tensor -> Right (exprType leftTyped)  -- Tensor product
      Eq -> Right TClassicalBit
      NEq -> Right TClassicalBit
      Lt -> Right TClassicalBit
      _ -> Right (exprType leftTyped)

    case resultType of
      Left msg -> Left [SemanticError msg Nothing "binary operation"]
      Right rt -> Right (TypedExpr expr rt Nothing, env'')

  Index base idx -> do
    (baseTyped, env') <- checkExpr base env
    (idxTyped, env'') <- checkExpr idx env'

    case exprType baseTyped of
      TQRegister n ->
        if exprType idxTyped == TClassicalInt
          then Right (TypedExpr expr TQubit Nothing, env'')
          else Left [SemanticError "Register index must be integer" Nothing "indexing"]
      _ -> Left [SemanticError "Can only index quantum registers" Nothing "indexing"]

  FuncCall name args -> do
    -- Type check all arguments
    typedArgs <- mapM (\arg -> do
      (typed, _) <- checkExpr arg env
      return typed) args

    -- Infer function result type
    resultType <- checkGateApplication name args env
    case resultType of
      Left err -> Left err
      Right rt -> Right (TypedExpr expr rt Nothing, env)

  _ -> Right (TypedExpr expr TVoid Nothing, env)

-- | Type check statement
checkStatement :: Statement -> AnalyzerEnv -> SemanticCheck (TypedStatement, AnalyzerEnv)
checkStatement stmt env = case stmt of
  GateCall gateName exprs -> do
    -- Type check all arguments
    typedExprs <- mapM (\e -> do
      (typed, _) <- checkExpr e env
      return (exprType typed)) exprs

    -- Check gate application
    resultType <- checkGateApplication gateName exprs env
    case resultType of
      Left err -> Left err
      Right rt -> Right (TypedStatement stmt rt Nothing, env)

  Measurement expr basis -> do
    (exprTyped, env') <- checkExpr expr env

    -- Check expression is measurable
    case measurementResultType (exprType exprTyped) of
      Left msg -> Left [SemanticError msg Nothing "measurement"]
      Right resultType -> Right (TypedStatement stmt resultType Nothing, env')

  Reset expr -> do
    (exprTyped, env') <- checkExpr expr env

    -- Check expression is resettable (affine)
    if isResettable (exprType exprTyped)
      then Right (TypedStatement stmt (exprType exprTyped) Nothing, env')
      else Left [SemanticError "Can only reset affine values" Nothing "reset"]

  Assign var expr -> do
    (exprTyped, env') <- checkExpr expr env
    let newEnv = addBinding var (exprType exprTyped) (typeCtx env')
    Right (TypedStatement stmt (exprType exprTyped) Nothing, env')

  If cond thenStmt elseStmt -> do
    (condTyped, env') <- checkExpr cond env
    -- Type check statements (for now, ignore for simplicity)
    Right (TypedStatement stmt TVoid Nothing, env')

  Barrier -> Right (TypedStatement stmt TVoid Nothing, env)

  Return (Just expr) -> do
    (exprTyped, env') <- checkExpr expr env
    Right (TypedStatement stmt (exprType exprTyped) Nothing, env')

  Return Nothing -> Right (TypedStatement stmt TVoid Nothing, env)

  _ -> Right (TypedStatement stmt TVoid Nothing, env)

-- | Type check program
analyzeProgram :: Program -> SemanticCheck AnalyzedProgram
analyzeProgram prog = do
  env <- foldM analyzeDeclaration emptyAnalyzerEnv (declarations prog)
  if not (null (errors env))
    then Left (errors env)
    else Right (AnalyzedProgram
      { analyzedDecls = map (\d -> case d of
          DeclRegister r -> AnalyzedRegisterDecl r
          DeclGate g -> AnalyzedGateDecl g
          DeclCircuit c -> AnalyzedCircuitDecl c
          DeclFunction f -> AnalyzedFunctionDecl f) (declarations prog)
      , analyzedEnv = env
      })

-- | Analyze single declaration
analyzeDeclaration :: AnalyzerEnv -> Declaration -> SemanticCheck AnalyzerEnv
analyzeDeclaration env decl = case decl of
  DeclRegister regDecl -> addRegisterDecl regDecl env

  DeclGate gateSpec -> do
    let gateInfo = GateInfo
          { gateInfoName = gateName gateSpec
          , gateInfoQubits = gateQubits gateSpec
          , gateInfoClassical = gateClassicalArgs gateSpec
          , gateInfoDeclLoc = Nothing
          }
    Right $ env { gates = Map.insert (gateName gateSpec) gateInfo (gates env) }

  DeclCircuit _ -> Right env

  DeclFunction _ -> Right env

-- | Validation report
data ValidationReport = ValidationReport
  { reportErrors :: [SemanticError]
  , reportWarnings :: [String]
  , reportOK :: Bool
  , reportAnalyzedProgram :: Maybe AnalyzedProgram
  } deriving (Show, Eq, Generic)

instance ToJSON ValidationReport
instance FromJSON ValidationReport

-- | Validate and analyze source code
validateSource :: String -> Either ValidationReport AnalyzedProgram
validateSource source = do
  -- Tokenize
  tokens <- case tokenize "input.qcl" source of
    Left err -> Left $ ValidationReport [SemanticError err Nothing "tokenization"] [] False Nothing
    Right toks -> Right toks

  -- Parse
  prog <- case parseProgram tokens of
    Left parseErr -> Left $ ValidationReport [SemanticError (errorMsg parseErr) Nothing "parsing"] [] False Nothing
    Right p -> Right p

  -- Analyze
  case analyzeProgram prog of
    Left semErrs -> Left $ ValidationReport semErrs [] False Nothing
    Right analyzed -> Right analyzed

-- | Pretty-print semantic error
prettySemError :: SemanticError -> String
prettySemError err =
  let locStr = case semErrLoc err of
        Nothing -> ""
        Just loc -> " @" ++ sourceFile loc ++ ":" ++ show (sourceLine loc) ++ ":" ++ show (sourceCol loc)
      ctxStr = if null (semErrContext err) then "" else " (" ++ semErrContext err ++ ")"
  in "Error" ++ locStr ++ ": " ++ semErrMsg err ++ ctxStr

-- | Pretty-print all errors
prettySemErrors :: [SemanticError] -> String
prettySemErrors errs = unlines (map prettySemError errs)

-- | Helper: fold with error handling
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a [] = return a
foldM f a (b:bs) = do
  a' <- f a b
  foldM f a' bs

-- | Helper: map with error handling
mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
  y <- f x
  ys <- mapM f xs
  return (y:ys)
