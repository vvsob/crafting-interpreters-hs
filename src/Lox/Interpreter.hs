module Lox.Interpreter (
    runStatements,
    eval   
) where

import Lox.Expr
import Lox.Scanner
import Lox.Environment
import Control.Monad.State
import Control.Monad

data InterpreterState = InterpreterState {io :: IO (), environment :: Environment}

emptyInterpreter :: InterpreterState
emptyInterpreter = InterpreterState {io=return (), environment=emptyEnvironment}

runStatements :: [Stmt] -> IO ()
runStatements s = io
    where InterpreterState {io=io} = execState (interpret s) emptyInterpreter

interpret :: [Stmt] -> State InterpreterState ()
interpret = foldr ((>>) . execute) (return ())

execute :: Stmt -> State InterpreterState ()
execute (BlockStmt statements) = executeBlock statements
execute (ExpressionStmt value) = void $ evalFrom value
execute (IfStmt condition thenBranch elseBranchMaybe) = do
    condValue <- isTruthy <$> evalFrom condition
    if condValue then execute thenBranch else forM_ elseBranchMaybe execute 
execute (PrintStmt expr) = do
    value <- evalFrom expr
    modify (\s@(InterpreterState {io=io}) -> s {io=io >> print value})
execute (VariableStmt name expr) = do
    value <- evalFrom expr
    modify (\s@(InterpreterState {environment=env}) -> s {environment=define (tokenLexeme name) value env})
execute (WhileStmt condition body) = executeWhile condition body 

executeBlock :: [Stmt] -> State InterpreterState ()
executeBlock statements = do 
    oldEnv <- gets environment
    modify (\s@InterpreterState {environment=_} -> s {environment=emptyEnvironment {enclosing=Just oldEnv}})
    interpret statements
    modify (\s@InterpreterState {environment=Environment {enclosing=Just enclosing}} -> s {environment=enclosing})

executeWhile :: Expr -> Stmt -> State InterpreterState ()
executeWhile condition body = do
    shouldContinue <- evalFrom condition
    when (isTruthy shouldContinue) $ execute body >> executeWhile condition body

eval :: Expr -> IO Object
eval expr = return $ evalState (evalFrom expr) emptyInterpreter

evalFrom :: Expr -> State InterpreterState Object
evalFrom (LiteralExpr value) = return value
evalFrom (VariableExpr name) = do
    maybeObject <- gets (\(InterpreterState {environment=env}) -> Lox.Environment.get (tokenLexeme name) env)
    case maybeObject of
        Nothing -> error "Undefined variable"
        Just object -> return object
evalFrom (AssignmentExpr name expr) = do
    value <- evalFrom expr
    success <- state $ f value
    if success then return value else error "Undefined variable"
    where f value s@InterpreterState {environment=env} = let (success, newEnv) = assign (tokenLexeme name) value env in (success, s {environment=newEnv})

evalFrom (GroupingExpr expr) = evalFrom expr
evalFrom (UnaryExpr op expr) = do 
    right <- evalFrom expr
    case (tokenType op, right) of
        (MINUS, NumberObject x) -> return $ NumberObject (-x)
        (BANG, object) -> return $ BoolObject $ not $ isTruthy object
        _ -> error "Type error"
evalFrom (LogicalExpr leftExpr op rightExpr) = do
    isLeftTruthy <- isTruthy <$> evalFrom leftExpr
    case (tokenType op, isLeftTruthy) of
        (OR, True) -> return $ BoolObject True
        (OR, False) -> evalFrom rightExpr
        (AND, True) -> evalFrom rightExpr
        (AND, False) -> return $ BoolObject False
        _ -> error "Unreachable"
evalFrom (BinaryExpr leftExpr op rightExpr) = do
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

isTruthy :: Object -> Bool
isTruthy NullObject = False
isTruthy (BoolObject False) = False
isTruthy _ = True
