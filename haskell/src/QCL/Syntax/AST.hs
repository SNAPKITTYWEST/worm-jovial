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

-- | Abstract Syntax Tree module
--
-- Re-exports AST types from QCL.Syntax.Parser for convenience.
-- All AST node types (Expr, Statement, Declaration, Program, etc.)
-- are defined in the Parser module and re-exported here.

module QCL.Syntax.AST
  ( -- * Expressions
    Expr(..)
  , Op(..)
  , UnaryOp(..)

    -- * Statements
  , Statement(..)
  , Basis(..)

    -- * Declarations
  , Declaration(..)
  , RegisterDecl(..)
  , RegisterType(..)
  , GateSpec(..)
  , CircuitDecl(..)
  , FunctionDecl(..)

    -- * Program
  , Program(..)

    -- * Parse errors
  , ParseError(..)
  ) where

import QCL.Syntax.Parser
