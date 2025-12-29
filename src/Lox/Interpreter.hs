module Lox.Interpreter (
    runStatements,
    eval   
) where

import Lox.Expr
import Lox.Scanner
import Control.Monad.State
import Control.Monad

data InterpreterState = InterpreterState (IO ())

emptyInterpreter :: InterpreterState
emptyInterpreter = InterpreterState (return ())

runStatements :: [Stmt] -> IO ()
runStatements s = io
    where InterpreterState io = execState (interpret s) emptyInterpreter

interpret :: [Stmt] -> State InterpreterState ()
interpret = foldr ((>>) . execute) (return ())

execute :: Stmt -> State InterpreterState ()
execute (Print expr) = do
    value <- evalFrom expr
    modify (\(InterpreterState s) -> InterpreterState (s >> print value))
execute (Expression value) = void $ evalFrom value

eval :: Expr -> IO Object
eval expr = return $ evalState (evalFrom expr) $ InterpreterState (return ())

evalFrom :: Expr -> State InterpreterState Object
evalFrom (Literal value) = return value
evalFrom (Grouping expr) = evalFrom expr
evalFrom (Unary op expr) = do 
    right <- evalFrom expr
    case (tokenType op, right) of
        (MINUS, NumberObject x) -> return $ NumberObject (-x)
        (BANG, NullObject) -> return $ BoolObject False
        (BANG, BoolObject x) -> return $ BoolObject (not x)
        (BANG, _) -> return $ BoolObject True
        _ -> error "Type error"
evalFrom (Binary leftExpr op rightExpr) = do
    left <- evalFrom leftExpr
    right <- evalFrom rightExpr
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

