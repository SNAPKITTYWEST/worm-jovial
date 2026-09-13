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

-- | Parser and Abstract Syntax Tree (AST) for QCL
--
-- Converts token stream into abstract syntax tree.
-- Grammar:
--   program      ::= declaration*
--   declaration  ::= register_decl | gate_decl | function_decl | circuit_decl
--   statement    ::= gate_call | measurement | reset | control_flow | assignment
--   expression   ::= binary_op | unary_op | primary

module QCL.Syntax.Parser where

import Data.Aeson
import Data.List (intercalate)
import GHC.Generics
import qualified Data.Map as Map

import QCL.Syntax.Lexer

-- | AST Node types

-- | Expression
data Expr
  = Var String                    -- ^ Variable reference
  | IntLit Int                    -- ^ Integer literal
  | FloatLit Double               -- ^ Float literal
  | StringLit String              -- ^ String literal
  | BinOp Op Expr Expr            -- ^ Binary operation
  | UnaryOp UnaryOp Expr          -- ^ Unary operation
  | Index Expr Expr               -- ^ Array/register indexing: e[i]
  | Slice Expr Expr Expr          -- ^ Slice: e[start:end]
  | FuncCall String [Expr]        -- ^ Function call
  | Tuple [Expr]                  -- ^ Tuple expression
  | Lambda [String] Expr          -- ^ Lambda function
  deriving (Show, Eq, Generic)

instance ToJSON Expr
instance FromJSON Expr

-- | Binary operator
data Op
  = Add | Sub | Mul | Div | Mod
  | Pow | Tensor
  | Eq | NEq | Lt | Gt | Lte | Gte
  | And | Or
  | Assign | PlusAssign | MinusAssign
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Op
instance FromJSON Op

-- | Unary operator
data UnaryOp
  = Neg | Not | Adjoint | Dagger | Sqrt | Conj
  deriving (Show, Eq, Ord, Generic)

instance ToJSON UnaryOp
instance FromJSON UnaryOp

-- | Gate specification
data GateSpec = GateSpec
  { gateName :: String
  , gateQubits :: [String]        -- ^ Qubit arguments
  , gateClassicalArgs :: [String] -- ^ Classical parameters
  , gateBody :: [Statement]       -- ^ Gate implementation
  } deriving (Show, Eq, Generic)

instance ToJSON GateSpec
instance FromJSON GateSpec

-- | Measurement basis
data Basis
  = ComputationalBasis
  | HadamardBasis
  | DiagonalBasis
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Basis
instance FromJSON Basis

-- | Statement
data Statement
  = GateCall String [Expr]        -- ^ Apply gate: H q[0]
  | Measurement Expr Basis        -- ^ Measure qubit
  | Reset Expr                    -- ^ Reset ancilla
  | Assign String Expr            -- ^ Assignment: x = e
  | If Expr [Statement] (Maybe [Statement])  -- ^ If-then-else
  | For String Expr Expr [Statement]  -- ^ For loop
  | While Expr [Statement]        -- ^ While loop
  | Control Expr [Statement]      -- ^ Control flow (if qubit is |1⟩)
  | Barrier                       -- ^ Synchronization barrier
  | Return (Maybe Expr)           -- ^ Return statement
  | Break                         -- ^ Break from loop
  | Continue                      -- ^ Continue loop
  | Block [Statement]             -- ^ Block of statements
  deriving (Show, Eq, Generic)

instance ToJSON Statement
instance FromJSON Statement

-- | Register declaration
data RegisterDecl = RegisterDecl
  { regName :: String
  , regType :: RegisterType
  , regSize :: Expr
  , regInit :: Maybe Expr
  } deriving (Show, Eq, Generic)

instance ToJSON RegisterDecl
instance FromJSON RegisterDecl

-- | Register type
data RegisterType
  = QuantumReg
  | AncillaReg
  | ClassicalReg
  deriving (Show, Eq, Ord, Generic)

instance ToJSON RegisterType
instance FromJSON RegisterType

-- | Circuit declaration
data CircuitDecl = CircuitDecl
  { circName :: String
  , circQubits :: [String]
  , circBody :: [Statement]
  } deriving (Show, Eq, Generic)

instance ToJSON CircuitDecl
instance FromJSON CircuitDecl

-- | Function declaration
data FunctionDecl = FunctionDecl
  { funcName :: String
  , funcParams :: [(String, String)]  -- ^ (name, type)
  , funcReturnType :: String
  , funcBody :: [Statement]
  } deriving (Show, Eq, Generic)

instance ToJSON FunctionDecl
instance FromJSON FunctionDecl

-- | Declaration (top-level)
data Declaration
  = DeclRegister RegisterDecl
  | DeclGate GateSpec
  | DeclCircuit CircuitDecl
  | DeclFunction FunctionDecl
  deriving (Show, Eq, Generic)

instance ToJSON Declaration
instance FromJSON Declaration

-- | Complete program
data Program = Program
  { declarations :: [Declaration]
  , sourceFile :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Program
instance FromJSON Program

-- | Parse error
data ParseError = ParseError
  { errorMsg :: String
  , errorToken :: Maybe Token
  , errorExpected :: String
  } deriving (Show, Eq, Generic)

instance ToJSON ParseError
instance FromJSON ParseError

-- | Parser state
data Parser = Parser
  { tokens :: [Token]
  , position :: Int
  } deriving (Show, Eq)

-- | Parser monad (Either for error handling)
type ParseResult a = Either ParseError a

-- | Initialize parser
initParser :: [Token] -> Parser
initParser toks = Parser toks 0

-- | Peek at current token
peekTok :: Parser -> Maybe Token
peekTok p =
  if position p < length (tokens p)
    then Just (tokens p !! position p)
    else Nothing

-- | Consume token (advance position)
consumeTok :: Parser -> Parser
consumeTok p = p { position = position p + 1 }

-- | Expect specific token type
expectToken :: TokenType -> Parser -> ParseResult (Token, Parser)
expectToken expected p =
  case peekTok p of
    Nothing -> Left $ ParseError "Unexpected end of input" Nothing (show expected)
    Just tok ->
      if tokenType tok == expected
        then Right (tok, consumeTok p)
        else Left $ ParseError ("Expected " ++ show expected) (Just tok) ""

-- | Parse primary expression
parsePrimary :: Parser -> ParseResult (Expr, Parser)
parsePrimary p =
  case peekTok p of
    Nothing -> Left $ ParseError "Unexpected end of input" Nothing "expression"

    Just tok -> case tokenType tok of
      IntLiteral n -> Right (IntLit n, consumeTok p)
      FloatLiteral f -> Right (FloatLit f, consumeTok p)
      StringLiteral s -> Right (StringLit s, consumeTok p)
      Identifier name -> Right (Var name, consumeTok p)

      Punc_LParen -> do
        let p' = consumeTok p
        (expr, p'') <- parseExpr p'
        (_, p''') <- expectToken Punc_RParen p''
        Right (expr, p''')

      _ -> Left $ ParseError "Expected expression" (Just tok) ""

-- | Parse binary operator
parseBinOp :: Op -> Expr -> Expr -> Expr
parseBinOp op left right = BinOp op left right

-- | Parse expression (with operator precedence)
parseExpr :: Parser -> ParseResult (Expr, Parser)
parseExpr p = do
  (left, p') <- parsePrimary p
  parseExprCont left p'

parseExprCont :: Expr -> Parser -> ParseResult (Expr, Parser)
parseExprCont left p =
  case peekTok p of
    Nothing -> Right (left, p)

    Just tok -> case tokenType tok of
      Op_Plus -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp Add left right
        parseExprCont expr p''

      Op_Minus -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp Sub left right
        parseExprCont expr p''

      Op_Star -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp Mul left right
        parseExprCont expr p''

      Op_Slash -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp Div left right
        parseExprCont expr p''

      Op_At -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp Tensor left right
        parseExprCont expr p''

      Op_Eq -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp Eq left right
        parseExprCont expr p''

      Op_NEq -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp NEq left right
        parseExprCont expr p''

      Op_Lt -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp Lt left right
        parseExprCont expr p''

      Op_And -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp And left right
        parseExprCont expr p''

      Op_Or -> do
        let p' = consumeTok p
        (right, p'') <- parsePrimary p'
        let expr = parseBinOp Or left right
        parseExprCont expr p''

      Punc_LBracket -> do
        let p' = consumeTok p
        (idx, p'') <- parseExpr p'
        (_, p''') <- expectToken Punc_RBracket p''
        let expr = Index left idx
        parseExprCont expr p'''

      _ -> Right (left, p)

-- | Parse statement
parseStatement :: Parser -> ParseResult (Statement, Parser)
parseStatement p =
  case peekTok p of
    Nothing -> Left $ ParseError "Unexpected end of input" Nothing "statement"

    Just tok -> case tokenType tok of
      KW_If -> do
        let p' = consumeTok p
        (_, p'') <- expectToken Punc_LParen p'
        (cond, p''') <- parseExpr p''
        (_, p'''') <- expectToken Punc_RParen p'''
        (thenStmt, p''''') <- parseStatement p''''
        let elseStmt = Nothing
        Right (If cond [thenStmt] elseStmt, p''''')

      KW_Measure -> do
        let p' = consumeTok p
        (expr, p'') <- parseExpr p'
        Right (Measurement expr ComputationalBasis, p'')

      KW_Reset -> do
        let p' = consumeTok p
        (expr, p'') <- parseExpr p'
        Right (Reset expr, p'')

      Identifier name -> do
        let p' = consumeTok p
        case peekTok p' of
          Just (Token Op_Assign _ _) -> do
            let p'' = consumeTok p'
            (expr, p''') <- parseExpr p''
            Right (Assign name expr, p''')
          _ -> do
            (exprs, p'') <- parseArgumentList p'
            Right (GateCall name exprs, p'')

      KW_Barrier -> do
        Right (Barrier, consumeTok p)

      KW_Return -> do
        let p' = consumeTok p
        case peekTok p' of
          Just (Token Punc_Semicolon _ _) -> Right (Return Nothing, p')
          _ -> do
            (expr, p'') <- parseExpr p'
            Right (Return (Just expr), p'')

      _ -> Left $ ParseError "Unexpected token in statement" (Just tok) ""

-- | Parse argument list (parenthesized expressions)
parseArgumentList :: Parser -> ParseResult ([Expr], Parser)
parseArgumentList p =
  case peekTok p of
    Just (Token Punc_LParen _ _) -> do
      let p' = consumeTok p
      parseArguments p' []
    _ -> Right ([], p)

-- | Parse arguments (helper)
parseArguments :: Parser -> [Expr] -> ParseResult ([Expr], Parser)
parseArguments p args =
  case peekTok p of
    Just (Token Punc_RParen _ _) -> Right (reverse args, consumeTok p)
    Just (Token Punc_Comma _ _) -> parseArguments (consumeTok p) args
    _ -> do
      (expr, p') <- parseExpr p
      parseArguments p' (expr : args)

-- | Parse declaration
parseDeclaration :: Parser -> ParseResult (Declaration, Parser)
parseDeclaration p =
  case peekTok p of
    Nothing -> Left $ ParseError "Unexpected end of input" Nothing "declaration"

    Just tok -> case tokenType tok of
      KW_Register -> do
        let p' = consumeTok p
        (name, p'') <- expectIdentifier p'
        (_, p''') <- expectToken Punc_LBracket p''
        (size, p'''') <- parseExpr p'''
        (_, p''''') <- expectToken Punc_RBracket p''''
        let regDecl = RegisterDecl name QuantumReg size Nothing
        Right (DeclRegister regDecl, p''''')

      KW_Gate -> do
        let p' = consumeTok p
        (name, p'') <- expectIdentifier p'
        (qubits, p''') <- parseQubitList p''
        (_, p'''') <- expectToken Punc_LBrace p'''
        (body, p''''') <- parseStatements p'''' []
        (_, p'''''') <- expectToken Punc_RBrace p'''''
        let gateSpec = GateSpec name qubits [] body
        Right (DeclGate gateSpec, p'''''')

      _ -> Left $ ParseError "Unexpected token in declaration" (Just tok) ""

-- | Parse qubit list
parseQubitList :: Parser -> ParseResult ([String], Parser)
parseQubitList p =
  case peekTok p of
    Just (Token Punc_LParen _ _) -> do
      let p' = consumeTok p
      parseQubits p' []
    _ -> Right ([], p)

-- | Parse qubits (helper)
parseQubits :: Parser -> [String] -> ParseResult ([String], Parser)
parseQubits p qubits =
  case peekTok p of
    Just (Token Punc_RParen _ _) -> Right (reverse qubits, consumeTok p)
    Just (Token Punc_Comma _ _) -> parseQubits (consumeTok p) qubits
    _ -> do
      (name, p') <- expectIdentifier p
      parseQubits p' (name : qubits)

-- | Parse statements (helper for block)
parseStatements :: Parser -> [Statement] -> ParseResult ([Statement], Parser)
parseStatements p stmts =
  case peekTok p of
    Just (Token Punc_RBrace _ _) -> Right (reverse stmts, p)
    _ -> do
      (stmt, p') <- parseStatement p
      case peekTok p' of
        Just (Token Punc_Semicolon _ _) -> parseStatements (consumeTok p') (stmt : stmts)
        _ -> parseStatements p' (stmt : stmts)

-- | Expect identifier token
expectIdentifier :: Parser -> ParseResult (String, Parser)
expectIdentifier p =
  case peekTok p of
    Just tok -> case tokenType tok of
      Identifier name -> Right (name, consumeTok p)
      _ -> Left $ ParseError "Expected identifier" (Just tok) ""
    Nothing -> Left $ ParseError "Unexpected end of input" Nothing "identifier"

-- | Parse program
parseProgram :: [Token] -> ParseResult Program
parseProgram toks = do
  let p = initParser toks
  (decls, _) <- parseDeclarations p []
  Right $ Program decls "unknown"

-- | Parse declarations (helper)
parseDeclarations :: Parser -> [Declaration] -> ParseResult ([Declaration], Parser)
parseDeclarations p decls =
  case peekTok p of
    Nothing -> Right (reverse decls, p)
    _ -> do
      (decl, p') <- parseDeclaration p
      parseDeclarations p' (decl : decls)

-- | Pretty-print AST
prettyExpr :: Expr -> String
prettyExpr expr = case expr of
  Var name -> name
  IntLit n -> show n
  FloatLit f -> show f
  StringLit s -> "\"" ++ s ++ "\""
  BinOp op left right -> "(" ++ prettyExpr left ++ " " ++ show op ++ " " ++ prettyExpr right ++ ")"
  UnaryOp op e -> show op ++ " " ++ prettyExpr e
  Index e i -> prettyExpr e ++ "[" ++ prettyExpr i ++ "]"
  FuncCall name args -> name ++ "(" ++ intercalate ", " (map prettyExpr args) ++ ")"
  _ -> "..."

-- | Parse with error reporting
parseWithErrors :: String -> Either ParseError Program
parseWithErrors source = do
  tokens <- tokenize "input.qcl" source
  parseProgram tokens
