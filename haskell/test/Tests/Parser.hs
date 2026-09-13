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

-- | Parser module tests
-- Tests lexer tokenization, expression parsing, statement parsing,
-- declaration parsing, and error reporting.

module Tests.Parser (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import QCL.Syntax.Lexer
import QCL.Syntax.Parser

tests :: TestTree
tests = testGroup "Parser"
  [ testGroup "Lexer tokenization"
      [ testCase "keywords are recognized" $ do
          case tokenize "test.qcl" "register gate measure reset if else" of
            Right toks -> do
              let types = map tokenType toks
              assertBool "register keyword" (KW_Register `elem` types)
              assertBool "gate keyword" (KW_Gate `elem` types)
              assertBool "measure keyword" (KW_Measure `elem` types)
              assertBool "reset keyword" (KW_Reset `elem` types)
              assertBool "if keyword" (KW_If `elem` types)
              assertBool "else keyword" (KW_Else `elem` types)
            Left err -> assertFailure err

      , testCase "identifiers are recognized" $ do
          case tokenize "test.qcl" "myVar foo_bar q0" of
            Right toks -> do
              length toks @?= 3
              case tokenType (toks !! 0) of
                Identifier "myVar" -> return ()
                other -> assertFailure $ "Expected Identifier myVar, got " ++ show other
              case tokenType (toks !! 1) of
                Identifier "foo_bar" -> return ()
                other -> assertFailure $ "Expected Identifier foo_bar, got " ++ show other
              case tokenType (toks !! 2) of
                Identifier "q0" -> return ()
                other -> assertFailure $ "Expected Identifier q0, got " ++ show other
            Left err -> assertFailure err

      , testCase "integer literals" $ do
          case tokenize "test.qcl" "42 0 100" of
            Right toks -> do
              length toks @?= 3
              case tokenType (toks !! 0) of
                IntLiteral 42 -> return ()
                other -> assertFailure $ "Expected IntLiteral 42, got " ++ show other
              case tokenType (toks !! 1) of
                IntLiteral 0 -> return ()
                other -> assertFailure $ "Expected IntLiteral 0, got " ++ show other
            Left err -> assertFailure err

      , testCase "string literals" $ do
          case tokenize "test.qcl" "\"hello world\"" of
            Right toks -> do
              length toks @?= 1
              case tokenType (toks !! 0) of
                StringLiteral "hello world" -> return ()
                other -> assertFailure $ "Expected StringLiteral, got " ++ show other
            Left err -> assertFailure err

      , testCase "operators" $ do
          case tokenize "test.qcl" "+ - * / == != <= >= && || =" of
            Right toks -> do
              let types = map tokenType toks
              assertBool "plus" (Op_Plus `elem` types)
              assertBool "minus" (Op_Minus `elem` types)
              assertBool "star" (Op_Star `elem` types)
              assertBool "slash" (Op_Slash `elem` types)
              assertBool "eq" (Op_Eq `elem` types)
              assertBool "neq" (Op_NEq `elem` types)
              assertBool "lte" (Op_Lte `elem` types)
              assertBool "gte" (Op_Gte `elem` types)
              assertBool "and" (Op_And `elem` types)
              assertBool "or" (Op_Or `elem` types)
              assertBool "assign" (Op_Assign `elem` types)
            Left err -> assertFailure err

      , testCase "punctuation" $ do
          case tokenize "test.qcl" "( ) [ ] { } ; , : ::" of
            Right toks -> do
              let types = map tokenType toks
              assertBool "lparen" (Punc_LParen `elem` types)
              assertBool "rparen" (Punc_RParen `elem` types)
              assertBool "lbracket" (Punc_LBracket `elem` types)
              assertBool "rbracket" (Punc_RBracket `elem` types)
              assertBool "lbrace" (Punc_LBrace `elem` types)
              assertBool "rbrace" (Punc_RBrace `elem` types)
              assertBool "semicolon" (Punc_Semicolon `elem` types)
              assertBool "comma" (Punc_Comma `elem` types)
              assertBool "colon" (Punc_Colon `elem` types)
              assertBool "doublecolon" (Punc_DoubleColon `elem` types)
            Left err -> assertFailure err

      , testCase "arrow operators" $ do
          case tokenize "test.qcl" "-> =>" of
            Right toks -> do
              let types = map tokenType toks
              assertBool "arrow" (Op_Arrow `elem` types)
              assertBool "fat arrow" (Op_FatArrow `elem` types)
            Left err -> assertFailure err

      , testCase "line comments are skipped" $ do
          case tokenize "test.qcl" "x // this is a comment\ny" of
            Right toks -> do
              let idents = [s | Identifier s <- map tokenType toks]
              assertBool "x present" ("x" `elem` idents)
              assertBool "y present" ("y" `elem` idents)
              assertBool "no comment tokens" (length toks == 2)
            Left err -> assertFailure err

      , testCase "block comments are skipped" $ do
          case tokenize "test.qcl" "x /* block */ y" of
            Right toks -> do
              let idents = [s | Identifier s <- map tokenType toks]
              length idents @?= 2
            Left err -> assertFailure err

      , testCase "empty input produces no tokens" $ do
          case tokenize "test.qcl" "" of
            Right toks -> length toks @?= 0
            Left err -> assertFailure err

      , testCase "whitespace-only input produces no tokens" $ do
          case tokenize "test.qcl" "   \n  \t  " of
            Right toks -> length toks @?= 0
            Left err -> assertFailure err
      ]

  , testGroup "Expression parsing"
      [ testCase "integer literal" $ do
          case tokenize "test.qcl" "42" of
            Right toks -> case parseExpr (initParser toks) of
              Right (IntLit 42, _) -> return ()
              Right (other, _) -> assertFailure $ "Expected IntLit 42, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "variable" $ do
          case tokenize "test.qcl" "myVar" of
            Right toks -> case parseExpr (initParser toks) of
              Right (Var "myVar", _) -> return ()
              Right (other, _) -> assertFailure $ "Expected Var myVar, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "binary addition" $ do
          case tokenize "test.qcl" "x + y" of
            Right toks -> case parseExpr (initParser toks) of
              Right (BinOp Add (Var "x") (Var "y"), _) -> return ()
              Right (other, _) -> assertFailure $ "Expected x + y, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "binary subtraction" $ do
          case tokenize "test.qcl" "a - b" of
            Right toks -> case parseExpr (initParser toks) of
              Right (BinOp Sub (Var "a") (Var "b"), _) -> return ()
              Right (other, _) -> assertFailure $ "Got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "chained operations associate left" $ do
          case tokenize "test.qcl" "a + b + c" of
            Right toks -> case parseExpr (initParser toks) of
              Right (BinOp Add (BinOp Add (Var "a") (Var "b")) (Var "c"), _) -> return ()
              Right (other, _) -> assertFailure $ "Expected left-association, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "parenthesized expression" $ do
          case tokenize "test.qcl" "(x)" of
            Right toks -> case parseExpr (initParser toks) of
              Right (Var "x", _) -> return ()
              Right (other, _) -> assertFailure $ "Got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "tensor product operator" $ do
          case tokenize "test.qcl" "a @ b" of
            Right toks -> case parseExpr (initParser toks) of
              Right (BinOp Tensor (Var "a") (Var "b"), _) -> return ()
              Right (other, _) -> assertFailure $ "Expected tensor, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "comparison operators" $ do
          case tokenize "test.qcl" "x == y" of
            Right toks -> case parseExpr (initParser toks) of
              Right (BinOp Eq (Var "x") (Var "y"), _) -> return ()
              Right (other, _) -> assertFailure $ "Got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "array indexing" $ do
          case tokenize "test.qcl" "q[0]" of
            Right toks -> case parseExpr (initParser toks) of
              Right (Index (Var "q") (IntLit 0), _) -> return ()
              Right (other, _) -> assertFailure $ "Expected q[0], got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err
      ]

  , testGroup "Statement parsing"
      [ testCase "gate call with arguments" $ do
          case tokenize "test.qcl" "H(q)" of
            Right toks -> case parseStatement (initParser toks) of
              Right (GateCall "H" [Var "q"], _) -> return ()
              Right (other, _) -> assertFailure $ "Expected H(q), got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "measure statement" $ do
          case tokenize "test.qcl" "measure q" of
            Right toks -> case parseStatement (initParser toks) of
              Right (Measurement (Var "q") ComputationalBasis, _) -> return ()
              Right (other, _) -> assertFailure $ "Expected measure q, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "reset statement" $ do
          case tokenize "test.qcl" "reset a" of
            Right toks -> case parseStatement (initParser toks) of
              Right (Reset (Var "a"), _) -> return ()
              Right (other, _) -> assertFailure $ "Expected reset a, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "if statement" $ do
          case tokenize "test.qcl" "if (x) barrier" of
            Right toks -> case parseStatement (initParser toks) of
              Right (If _ [Barrier] Nothing, _) -> return ()
              Right (other, _) -> assertFailure $ "Expected if, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "barrier statement" $ do
          case tokenize "test.qcl" "barrier" of
            Right toks -> case parseStatement (initParser toks) of
              Right (Barrier, _) -> return ()
              Right (other, _) -> assertFailure $ "Expected barrier, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "return statement with expression" $ do
          case tokenize "test.qcl" "return x" of
            Right toks -> case parseStatement (initParser toks) of
              Right (Return (Just (Var "x")), _) -> return ()
              Right (other, _) -> assertFailure $ "Expected return x, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "assignment statement" $ do
          case tokenize "test.qcl" "x = 42" of
            Right toks -> case parseStatement (initParser toks) of
              Right (Assign "x" (IntLit 42), _) -> return ()
              Right (other, _) -> assertFailure $ "Expected x = 42, got " ++ show other
              Left err -> assertFailure $ show err
            Left err -> assertFailure err
      ]

  , testGroup "Declaration parsing"
      [ testCase "register declaration" $ do
          case tokenize "test.qcl" "register q[5]" of
            Right toks -> case parseDeclaration (initParser toks) of
              Right (DeclRegister rd, _) -> do
                regName rd @?= "q"
                regType rd @?= QuantumReg
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "gate declaration with body" $ do
          case tokenize "test.qcl" "gate myGate(q) { barrier }" of
            Right toks -> case parseDeclaration (initParser toks) of
              Right (DeclGate gs, _) -> do
                gateName gs @?= "myGate"
                length (gateQubits gs) @?= 1
              Left err -> assertFailure $ show err
            Left err -> assertFailure err
      ]

  , testGroup "Error reporting"
      [ testCase "malformed expression reports error" $ do
          case tokenize "test.qcl" "+" of
            Right toks -> case parseExpr (initParser toks) of
              Left _ -> return ()  -- Expected error
              Right _ -> assertFailure "Should fail on bare +"
            Left err -> assertFailure err

      , testCase "unexpected token in declaration" $ do
          case tokenize "test.qcl" "42" of
            Right toks -> case parseDeclaration (initParser toks) of
              Left _ -> return ()  -- Expected error
              Right _ -> assertFailure "Should fail: 42 is not a declaration"
            Left err -> assertFailure err

      , testCase "parseWithErrors returns Left on bad input" $ do
          case parseWithErrors "42 42 42" of
            Left _ -> return ()  -- Expected: not a valid program
            Right _ -> assertFailure "Should fail on invalid program"
      ]

  , testGroup "Full parse pipeline"
      [ testCase "simple program parses" $ do
          let src = "register q[3]"
          case tokenize "test.qcl" src of
            Right toks -> case parseProgram toks of
              Right prog -> do
                length (declarations prog) @?= 1
              Left err -> assertFailure $ show err
            Left err -> assertFailure err

      , testCase "multi-declaration program" $ do
          let src = "register q[3] register a[2]"
          case tokenize "test.qcl" src of
            Right toks -> case parseProgram toks of
              Right prog -> do
                length (declarations prog) @?= 2
              Left err -> assertFailure $ show err
            Left err -> assertFailure err
      ]
  ]
