module Lox.Expr (
    Stmt (..),
    Expr (..)
) where

import Lox.Scanner

data Stmt =
    Expression Expr |
    Print Expr
    deriving Show

data Expr = 
    Literal Object | 
    Unary Token Expr | 
    Binary Expr Token Expr | 
    Grouping Expr
    deriving Show
