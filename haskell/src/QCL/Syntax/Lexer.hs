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

-- | Lexer for Quantum Cryptographic Language
--
-- Tokenization of QCL source code with source location tracking.
--
-- Tokens:
--   - Keywords: register, ancilla, gate, measure, reset, if, for, while, etc.
--   - Identifiers: Names for registers, operations, variables
--   - Literals: Numbers (integers, floats, complex)
--   - Operators: +, -, *, /, @, >, <, ==, etc.
--   - Punctuation: (, ), [, ], {, }, ;, ,, :, .
--   - Comments: // line comments, /* */ block comments

module QCL.Syntax.Lexer where

import Data.Aeson
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum)
import Data.List (isPrefixOf)
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Source location (file, line, column)
data SourceLoc = SourceLoc
  { sourceFile :: String
  , sourceLine :: Int
  , sourceCol :: Int
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON SourceLoc
instance FromJSON SourceLoc

-- | Token type
data TokenType
  -- Keywords
  = KW_Register
  | KW_Quantum
  | KW_Ancilla
  | KW_Classical
  | KW_Gate
  | KW_Measure
  | KW_Reset
  | KW_If
  | KW_Else
  | KW_For
  | KW_While
  | KW_Do
  | KW_Break
  | KW_Continue
  | KW_Return
  | KW_Function
  | KW_Procedure
  | KW_Let
  | KW_In
  | KW_Circuit
  | KW_Import
  | KW_Export
  | KW_Type
  | KW_Const
  | KW_Mut
  | KW_Match
  | KW_With

  -- Identifiers and literals
  | Identifier String
  | IntLiteral Int
  | FloatLiteral Double
  | StringLiteral String
  | ComplexLiteral Double Double  -- Real + Imaginary

  -- Operators
  | Op_Plus
  | Op_Minus
  | Op_Star
  | Op_Slash
  | Op_Percent
  | Op_Hat                -- ^ (exponentiation)
  | Op_At                 -- @ (tensor product)
  | Op_Dagger             -- † (adjoint)
  | Op_Dot
  | Op_Eq                 -- ==
  | Op_NEq                -- !=
  | Op_Lt
  | Op_Gt
  | Op_Lte                -- <=
  | Op_Gte                -- >=
  | Op_And                -- &&
  | Op_Or                 -- ||
  | Op_Not                -- !
  | Op_Assign             -- =
  | Op_PlusEq             -- +=
  | Op_MinusEq            -- -=
  | Op_Arrow              -- ->
  | Op_FatArrow           -- =>
  | Op_Pipe               -- |
  | Op_Tilde              -- ~

  -- Punctuation
  | Punc_LParen
  | Punc_RParen
  | Punc_LBracket
  | Punc_RBracket
  | Punc_LBrace
  | Punc_RBrace
  | Punc_Semicolon
  | Punc_Comma
  | Punc_Colon
  | Punc_DoubleColon      -- ::
  | Punc_Question

  -- Special
  | EOF
  | Newline
  | Error String

  deriving (Show, Eq, Ord, Generic)

instance ToJSON TokenType
instance FromJSON TokenType

-- | Complete token with type and location
data Token = Token
  { tokenType :: TokenType
  , tokenValue :: String
  , tokenLoc :: SourceLoc
  } deriving (Show, Eq, Generic)

instance ToJSON Token
instance FromJSON Token

-- | Lexer state
data LexerState = LexerState
  { input :: String
  , position :: Int
  , line :: Int
  , column :: Int
  , fileName :: String
  , tokens :: [Token]
  } deriving (Show, Eq)

-- | Keyword set
keywords :: Set.Set String
keywords = Set.fromList
  [ "register", "quantum", "ancilla", "classical"
  , "gate", "measure", "reset"
  , "if", "else", "for", "while", "do", "break", "continue", "return"
  , "function", "procedure", "let", "in"
  , "circuit", "import", "export", "type", "const", "mut"
  , "match", "with"
  ]

-- | Check if string is keyword
isKeyword :: String -> Bool
isKeyword s = Set.member s keywords

-- | Map keyword string to token type
keywordToToken :: String -> TokenType
keywordToToken kw = case kw of
  "register" -> KW_Register
  "quantum" -> KW_Quantum
  "ancilla" -> KW_Ancilla
  "classical" -> KW_Classical
  "gate" -> KW_Gate
  "measure" -> KW_Measure
  "reset" -> KW_Reset
  "if" -> KW_If
  "else" -> KW_Else
  "for" -> KW_For
  "while" -> KW_While
  "do" -> KW_Do
  "break" -> KW_Break
  "continue" -> KW_Continue
  "return" -> KW_Return
  "function" -> KW_Function
  "procedure" -> KW_Procedure
  "let" -> KW_Let
  "in" -> KW_In
  "circuit" -> KW_Circuit
  "import" -> KW_Import
  "export" -> KW_Export
  "type" -> KW_Type
  "const" -> KW_Const
  "mut" -> KW_Mut
  "match" -> KW_Match
  "with" -> KW_With
  _ -> Identifier kw

-- | Initialize lexer state
initLexer :: String -> String -> LexerState
initLexer fileName input = LexerState
  { input = input
  , position = 0
  , line = 1
  , column = 1
  , fileName = fileName
  , tokens = []
  }

-- | Peek at next character
peekChar :: LexerState -> Maybe Char
peekChar state =
  if position state < length (input state)
    then Just (input state !! position state)
    else Nothing

-- | Peek at next n characters
peekString :: Int -> LexerState -> String
peekString n state =
  let start = position state
      end = min (start + n) (length (input state))
  in take n (drop start (input state))

-- | Advance by one character
advance :: LexerState -> LexerState
advance state =
  case peekChar state of
    Nothing -> state
    Just '\n' -> state
      { position = position state + 1
      , line = line state + 1
      , column = 1
      }
    Just _ -> state
      { position = position state + 1
      , column = column state + 1
      }

-- | Get current source location
currentLoc :: LexerState -> SourceLoc
currentLoc state = SourceLoc
  { sourceFile = fileName state
  , sourceLine = line state
  , sourceCol = column state
  }

-- | Add token to lexer state
addToken :: TokenType -> String -> LexerState -> LexerState
addToken ttype value state =
  let token = Token ttype value (currentLoc state)
  in state { tokens = tokens state ++ [token] }

-- | Skip whitespace
skipWhitespace :: LexerState -> LexerState
skipWhitespace state =
  case peekChar state of
    Just c | isSpace c -> skipWhitespace (advance state)
    _ -> state

-- | Skip line comment
skipLineComment :: LexerState -> LexerState
skipLineComment state =
  case peekChar state of
    Nothing -> state
    Just '\n' -> state
    _ -> skipLineComment (advance state)

-- | Skip block comment
skipBlockComment :: LexerState -> LexerState
skipBlockComment state =
  case (peekChar state, peekString 2 state) of
    (_, "*/") -> advance (advance state)
    (Nothing, _) -> state
    _ -> skipBlockComment (advance state)

-- | Read identifier or keyword
readIdentifier :: LexerState -> (String, LexerState)
readIdentifier state =
  case peekChar state of
    Just c | isAlphaNum c || c == '_' ->
      let (rest, state') = readIdentifier (advance state)
      in (c : rest, state')
    _ -> ("", state)

-- | Read number (integer or float)
readNumber :: LexerState -> (String, LexerState)
readNumber state =
  case peekChar state of
    Just c | isDigit c ->
      let (rest, state') = readNumber (advance state)
      in (c : rest, state')
    Just '.' ->
      case peekString 2 state of
        ".." -> ("", state)  -- Range operator, not decimal point
        _ -> let (rest, state') = readNumber (advance state)
             in ('.' : rest, state')
    _ -> ("", state)

-- | Read string literal (double-quoted)
readString :: LexerState -> (String, LexerState)
readString state =
  case peekChar state of
    Just '"' -> ("", advance state)  -- End of string
    Just '\\' -> case peekString 2 state of
      ('\\':c:_) ->
        let (rest, state') = readString (advance (advance state))
        in (c : rest, state')
      _ -> let (rest, state') = readString (advance state)
           in ('\\' : rest, state')
    Just c -> let (rest, state') = readString (advance state)
              in (c : rest, state')
    Nothing -> ("", state)

-- | Main lexer loop
runLexer :: LexerState -> Either String [Token]
runLexer state =
  let state' = skipWhitespace state
  in case peekChar state' of
    Nothing -> Right (tokens state')

    Just '/' -> case peekString 2 state' of
      "//" -> lex (skipLineComment (advance (advance state')))
      "/*" -> lex (skipBlockComment (advance (advance state')))
      _ -> lex (addToken Op_Slash "/" (advance state'))

    Just '+' -> case peekString 2 state' of
      "+=" -> lex (addToken Op_PlusEq "+=" (advance (advance state')))
      _ -> lex (addToken Op_Plus "+" (advance state'))

    Just '-' -> case peekString 2 state' of
      "->" -> lex (addToken Op_Arrow "->" (advance (advance state')))
      _ -> lex (addToken Op_Minus "-" (advance state'))

    Just '*' -> lex (addToken Op_Star "*" (advance state'))
    Just '%' -> lex (addToken Op_Percent "%" (advance state'))
    Just '^' -> lex (addToken Op_Hat "^" (advance state'))
    Just '@' -> lex (addToken Op_At "@" (advance state'))
    Just '~' -> lex (addToken Op_Tilde "~" (advance state'))

    Just '(' -> lex (addToken Punc_LParen "(" (advance state'))
    Just ')' -> lex (addToken Punc_RParen ")" (advance state'))
    Just '[' -> lex (addToken Punc_LBracket "[" (advance state'))
    Just ']' -> lex (addToken Punc_RBracket "]" (advance state'))
    Just '{' -> lex (addToken Punc_LBrace "{" (advance state'))
    Just '}' -> lex (addToken Punc_RBrace "}" (advance state'))
    Just ';' -> lex (addToken Punc_Semicolon ";" (advance state'))
    Just ',' -> lex (addToken Punc_Comma "," (advance state'))
    Just '?' -> lex (addToken Punc_Question "?" (advance state'))

    Just ':' -> case peekString 2 state' of
      "::" -> lex (addToken Punc_DoubleColon "::" (advance (advance state')))
      _ -> lex (addToken Punc_Colon ":" (advance state'))

    Just '.' -> lex (addToken Op_Dot "." (advance state'))

    Just '=' -> case peekString 2 state' of
      "==" -> lex (addToken Op_Eq "==" (advance (advance state')))
      "=>" -> lex (addToken Op_FatArrow "=>" (advance (advance state')))
      _ -> lex (addToken Op_Assign "=" (advance state'))

    Just '!' -> case peekString 2 state' of
      "!=" -> lex (addToken Op_NEq "!=" (advance (advance state')))
      _ -> lex (addToken Op_Not "!" (advance state'))

    Just '<' -> case peekString 2 state' of
      "<=" -> lex (addToken Op_Lte "<=" (advance (advance state')))
      _ -> lex (addToken Op_Lt "<" (advance state'))

    Just '>' -> case peekString 2 state' of
      ">=" -> lex (addToken Op_Gte ">=" (advance (advance state')))
      _ -> lex (addToken Op_Gt ">" (advance state'))

    Just '&' -> case peekString 2 state' of
      "&&" -> lex (addToken Op_And "&&" (advance (advance state')))
      _ -> lex (addToken (Error "Unexpected character: &") "&" (advance state'))

    Just '|' -> case peekString 2 state' of
      "||" -> lex (addToken Op_Or "||" (advance (advance state')))
      _ -> lex (addToken Op_Pipe "|" (advance state'))

    Just '†' -> lex (addToken Op_Dagger "†" (advance state'))

    Just '"' -> do
      let (str, state'') = readString (advance state')
      lex (addToken (StringLiteral str) ("\"" ++ str ++ "\"") state'')

    Just c | isAlpha c || c == '_' -> do
      let (ident, state'') = readIdentifier (advance state')
      let fullIdent = c : ident
      let tokenType = if isKeyword fullIdent then keywordToToken fullIdent else Identifier fullIdent
      lex (addToken tokenType fullIdent state'')

    Just c | isDigit c -> do
      let (numStr, state'') = readNumber (advance state')
      let fullNum = c : numStr
      case reads fullNum of
        [(n, "")] -> lex (addToken (IntLiteral n) fullNum state'')
        _ -> case reads fullNum of
          [(f, "")] -> lex (addToken (FloatLiteral f) fullNum state'')
          _ -> lex (addToken (Error ("Invalid number: " ++ fullNum)) fullNum state'')

    Just c -> lex (addToken (Error ("Unexpected character: " ++ [c])) [c] (advance state'))

-- | Tokenize source code
tokenize :: String -> String -> Either String [Token]
tokenize fileName source = lex (initLexer fileName source)

-- | Pretty-print token
prettyToken :: Token -> String
prettyToken token =
  let typeStr = case tokenType token of
        Identifier s -> "ID(" ++ s ++ ")"
        IntLiteral n -> "INT(" ++ show n ++ ")"
        FloatLiteral f -> "FLOAT(" ++ show f ++ ")"
        StringLiteral s -> "STR(" ++ s ++ ")"
        Error e -> "ERR(" ++ e ++ ")"
        tt -> show tt
      loc = tokenLoc token
  in typeStr ++ " @" ++ show (sourceLine loc) ++ ":" ++ show (sourceCol loc)

-- | Pretty-print token list
prettyTokens :: [Token] -> String
prettyTokens = unlines . map prettyToken

-- | Token stream type
type TokenStream = [Token]

-- | Get next token from stream
nextToken :: TokenStream -> Maybe (Token, TokenStream)
nextToken [] = Nothing
nextToken (t:ts) = Just (t, ts)

-- | Peek at next token
peekToken :: TokenStream -> Maybe Token
peekToken [] = Nothing
peekToken (t:_) = Just t

-- | Check token type
isTokenType :: TokenType -> Token -> Bool
isTokenType expected token = case (tokenType token, expected) of
  (Identifier _, Identifier _) -> True
  (a, b) -> a == b

-- | Get identifier value
getIdentifier :: Token -> Maybe String
getIdentifier token = case tokenType token of
  Identifier s -> Just s
  _ -> Nothing

-- | Get integer value
getIntLiteral :: Token -> Maybe Int
getIntLiteral token = case tokenType token of
  IntLiteral n -> Just n
  _ -> Nothing

-- | Get float value
getFloatLiteral :: Token -> Maybe Double
getFloatLiteral token = case tokenType token of
  FloatLiteral f -> Just f
  _ -> Nothing

-- | Get string value
getStringLiteral :: Token -> Maybe String
getStringLiteral token = case tokenType token of
  StringLiteral s -> Just s
  _ -> Nothing
