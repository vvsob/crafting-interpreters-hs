module Lox.Parser (
    parse
) where

import Control.Monad
import Control.Monad.State
import Lox.Scanner
import Lox.Expr

data ParserState = ParserState {tokens :: [Token]}

-- expression     → equality ;
-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- term           → factor ( ( "-" | "+" ) factor )* ;
-- factor         → unary ( ( "/" | "*" ) unary )* ;
-- unary          → ( "!" | "-" ) unary
--                | primary ;
-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--                | "(" expression ")" ;

parse :: [Token] -> Expr
parse tokens = evalState expression (ParserState {tokens=tokens})

expression :: State ParserState Expr
expression = equality

equality :: State ParserState Expr
equality = do
    expr <- comparison
    mergeExpressionMaybe expr <$> matchTail [BANG_EQUAL, EQUAL_EQUAL] comparison

comparison :: State ParserState Expr
comparison = do
    expr <- term
    mergeExpressionMaybe expr <$> matchTail [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] term

term :: State ParserState Expr
term = do
    expr <- factor
    mergeExpressionMaybe expr <$> matchTail [MINUS, PLUS] factor

factor :: State ParserState Expr
factor = do
    expr <- unary
    mergeExpressionMaybe expr <$> matchTail [SLASH, STAR] unary

unary :: State ParserState Expr
unary = do
    maybeOperator <- matchToken [BANG, MINUS]
    case maybeOperator of
        Nothing -> primary
        Just op -> Unary op <$> unary

primary :: State ParserState Expr
primary = do
    token <- advance
    case tokenType token of
        FALSE -> return $ Literal $ BoolObject False
        TRUE -> return $ Literal $ BoolObject True
        NIL -> return $ Literal NullObject
        NUMBER -> return $ Literal $ tokenObject token
        STRING -> return $ Literal $ tokenObject token
        LEFT_PAREN -> do
            expr <- expression
            consume RIGHT_PAREN "Expected '(' after ')'"
            return $ Grouping expr
        _ -> error "Expected expression"

matchTail :: [TokenType] -> State ParserState Expr -> State ParserState (Maybe (Token, Expr))
matchTail tokenTypes f = do
    maybeOperator <- matchToken tokenTypes
    case maybeOperator of
        Nothing -> return Nothing
        Just op -> do
            expr <- comparison
            rest <- matchTail tokenTypes f
            return $ Just (op, mergeExpressionMaybe expr rest)
mergeExpressionMaybe :: Expr -> Maybe (Token, Expr) -> Expr
mergeExpressionMaybe expr Nothing = expr
mergeExpressionMaybe left (Just (op, right)) = Binary left op right

matchToken :: [TokenType] -> State ParserState (Maybe Token)
matchToken [] = return Nothing
matchToken (t:ts) = do
    isMatch <- check t
    if isMatch then Just <$> advance else matchToken ts

check :: TokenType -> State ParserState Bool
check t = do
    atEnd <- isAtEnd
    if atEnd then return False else (== t) . tokenType <$> peek

consume :: TokenType -> String -> State ParserState Token
consume t msg = do
    isOk <- check t
    if isOk then advance else error msg

advance :: State ParserState Token
advance = state (\s@ParserState {tokens=(t:ts)} -> (t, s {tokens = ts}))

peek :: State ParserState Token
peek = gets (head . tokens) 

isAtEnd :: State ParserState Bool
isAtEnd = (== EOF) . tokenType <$> peek
