module Lox.Expr (
    Stmt (..),
    Expr (..)
) where

import Lox.Scanner

data Stmt =
    BlockStmt [Stmt] |
    ExpressionStmt Expr |
    IfStmt Expr Stmt (Maybe Stmt) |
    PrintStmt Expr |
    VariableStmt Token Expr |
    WhileStmt Expr Stmt
    deriving Show

data Expr = 
    LiteralExpr Object | 
    LogicalExpr Expr Token Expr |
    UnaryExpr Token Expr | 
    BinaryExpr Expr Token Expr | 
    GroupingExpr Expr |
    VariableExpr Token |
    AssignmentExpr Token Expr
    deriving Show
