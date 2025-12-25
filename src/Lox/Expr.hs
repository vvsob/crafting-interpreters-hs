module Lox.Expr (
    Expr (..)
) where

import Lox.Scanner

data Expr = 
    Literal Object | 
    Unary Token Expr | 
    Binary Expr Token Expr | 
    Grouping Expr

