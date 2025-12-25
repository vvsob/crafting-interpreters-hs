module Lox.Interpreter (
    eval   
) where

import Lox.Expr
import Lox.Scanner
import Control.Monad.State

data InterpreterState = InterpreterState

eval :: Expr -> Object
eval expr = evalState (interpret expr) InterpreterState

interpret :: Expr -> State InterpreterState Object
interpret (Literal value) = return value
interpret (Grouping expr) = interpret expr
interpret (Unary op expr) = do 
    right <- interpret expr
    case (tokenType op, right) of
        (MINUS, NumberObject x) -> return $ NumberObject (-x)
        (BANG, NullObject) -> return $ BoolObject False
        (BANG, BoolObject x) -> return $ BoolObject (not x)
        (BANG, _) -> return $ BoolObject True
        _ -> error "Type error"
interpret (Binary leftExpr op rightExpr) = do
    left <- interpret leftExpr
    right <- interpret rightExpr
    case (tokenType op, left, right) of
        (PLUS, NumberObject x, NumberObject y) -> return $ NumberObject (x + y)
        (MINUS, NumberObject x, NumberObject y) -> return $ NumberObject (x - y)
        (SLASH, NumberObject x, NumberObject y) -> return $ NumberObject (x / y)
        (STAR, NumberObject x, NumberObject y) -> return $ NumberObject (x * y)

        (GREATER, NumberObject x, NumberObject y) -> return $ BoolObject (x > y)
        (GREATER_EQUAL, NumberObject x, NumberObject y) -> return $ BoolObject (x >= y)
        (LESS, NumberObject x, NumberObject y) -> return $ BoolObject (x < y)
        (LESS_EQUAL, NumberObject x, NumberObject y) -> return $ BoolObject (x <= y)

        (PLUS, StringObject s, StringObject t) -> return $ StringObject (s ++ t)

        (EQUAL_EQUAL, x, y) -> return $ BoolObject (x == y)
        
        _ -> error "Type error"

