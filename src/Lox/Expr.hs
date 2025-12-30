module Lox.Expr (
    Stmt (..),
    Expr (..)
) where

import Lox.Scanner

data Stmt =
    BlockStmt [Stmt] |
    ExpressionStmt Expr |
    PrintStmt Expr |
    VariableStmt Token Expr
    deriving Show

data Expr = 
    LiteralExpr Object | 
    UnaryExpr Token Expr | 
    BinaryExpr Expr Token Expr | 
    GroupingExpr Expr |
    VariableExpr Token |
    AssignmentExpr Token Expr
    deriving Show
