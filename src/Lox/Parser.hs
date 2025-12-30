module Lox.Parser (
    ParserError (..),
    parse
) where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Maybe
import Lox.Scanner
import Lox.Expr

data ParserState = ParserState {tokens :: [Token]}

data ParserError = MismatchedParenthesesError 
                 | ExpectedExpressionError 
                 | ExpectedSemicolonError
                 | ExpectedVariableNameError
                 | ExpectedBraceAfterBlockError
                 | InvalidAssignmentTargetError
    deriving Show

-- program        → declaration* EOF ;
--
-- declaration    → varDecl
--                | statement ;
--
-- statement      → exprStmt
--                | printStmt 
--                | block ;
--
-- exprStmt       → expression ";" ;
-- printStmt      → "print" expression ";" ;
-- varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
-- block          → "{" declaration* "}" ;

-- expression     → assignment ;
-- assignment     → IDENTIFIER "=" assignment 
--                | equality ;
-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- term           → factor ( ( "-" | "+" ) factor )* ;
-- factor         → unary ( ( "/" | "*" ) unary )* ;
-- unary          → ( "!" | "-" ) unary
--                | primary ;
-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--                | "(" expression ")" | IDENTIFIER;

parse :: [Token] -> Either ParserError [Stmt]
parse tokens = evalState program (ParserState {tokens=tokens})

program :: State ParserState (Either ParserError [Stmt])
program = do
    atEnd <- isAtEnd
    if atEnd then return $ Right [] else do
        headMaybe <- declaration
        case headMaybe of
            Left err -> return $ Left err
            Right head -> do
                tailMaybe <- program
                case tailMaybe of
                    Left err -> return $ Left err
                    Right tail -> return $ Right $ head : tail
declaration :: State ParserState (Either ParserError Stmt)
declaration = do
    varMaybe <- matchToken [VAR]
    case varMaybe of
        Just _ -> varDeclaration
        _ -> statement

varDeclaration :: State ParserState (Either ParserError Stmt)
varDeclaration = do
    maybeName <- consume IDENTIFIER ExpectedVariableNameError
    case maybeName of
        Left err -> return $ Left err
        Right name -> do
            hasInit <- isJust <$> matchToken [EQUAL]
            initMaybe <- if hasInit then expression else return $ Right $ LiteralExpr NullObject
            semicolonMaybe <- consume SEMICOLON ExpectedSemicolonError
            case (initMaybe, semicolonMaybe) of
                (Left err, _) -> return $ Left err
                (_, Left err) -> return $ Left err
                (Right init, Right _) -> return $ Right $ VariableStmt name init


statement :: State ParserState (Either ParserError Stmt)
statement = do
    tokenMaybe <- matchToken [PRINT, LEFT_BRACE]
    case tokenMaybe of
        Just (Token {tokenType=PRINT}) -> printStatement
        Just (Token {tokenType=LEFT_BRACE}) -> do 
            result <- fmap BlockStmt <$> block
            braceMaybe <- consume RIGHT_BRACE ExpectedBraceAfterBlockError
            return $ braceMaybe >> result
        _ -> expressionStatement

block :: State ParserState (Either ParserError [Stmt])
block = do
    isRightBrace <- check RIGHT_BRACE
    if isRightBrace then return $ Right [] else do
        declMaybe <- declaration
        tailMaybe <- block
        case (declMaybe, tailMaybe) of
            (Left err, _) -> return $ Left err
            (_, Left err) -> return $ Left err
            (Right decl, Right tail) -> return $ Right $ decl : tail

printStatement :: State ParserState (Either ParserError Stmt)
printStatement = do
    valueMaybe <- expression
    semicolonMaybe <- consume SEMICOLON ExpectedSemicolonError
    case (valueMaybe, semicolonMaybe) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right value, Right _) -> return $ Right $ PrintStmt value

expressionStatement :: State ParserState (Either ParserError Stmt)
expressionStatement = do
    valueMaybe <- expression
    semicolonMaybe <- consume SEMICOLON ExpectedSemicolonError
    case (valueMaybe, semicolonMaybe) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right value, Right _) -> return $ Right $ ExpressionStmt value
    

expression :: State ParserState (Either ParserError Expr)
expression = assignment

assignment :: State ParserState (Either ParserError Expr)
assignment = do
    maybeExpr <- equality
    matchedEqual <- isJust <$> matchToken [EQUAL]
    if matchedEqual then do
        maybeValue <- assignment
        case (maybeExpr, maybeValue) of
            (Left err, _) -> return $ Left err
            (_, Left err) -> return $ Left err
            (Right (VariableExpr name), Right value) -> return $ Right $ AssignmentExpr name value
            _ -> return $ Left InvalidAssignmentTargetError
    else return maybeExpr

equality :: State ParserState (Either ParserError Expr)
equality = do
    exprMaybe <- comparison
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionMaybe expr) <$> matchTail [BANG_EQUAL, EQUAL_EQUAL] comparison

comparison :: State ParserState (Either ParserError Expr)
comparison = do
    exprMaybe <- term 
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionMaybe expr) <$> matchTail [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] term

term :: State ParserState (Either ParserError Expr)
term = do
    exprMaybe <- factor
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionMaybe expr) <$> matchTail [MINUS, PLUS] factor

factor :: State ParserState (Either ParserError Expr)
factor = do
    exprMaybe <- unary
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionMaybe expr) <$> matchTail [SLASH, STAR] unary

unary :: State ParserState (Either ParserError Expr)
unary = do
    maybeOperator <- matchToken [BANG, MINUS]
    case maybeOperator of
        Nothing -> primary
        Just op -> do
            exprMaybe <- unary
            case exprMaybe of
                Left err -> return $ Left err
                Right expr -> return $ Right $ UnaryExpr op expr

primary :: State ParserState (Either ParserError Expr)
primary = do
    token <- advance
    case tokenType token of
        FALSE -> return $ Right $ LiteralExpr $ BoolObject False
        TRUE -> return $ Right $ LiteralExpr $ BoolObject True
        NIL -> return $ Right $ LiteralExpr NullObject
        NUMBER -> return $ Right $ LiteralExpr $ tokenObject token
        STRING -> return $ Right $ LiteralExpr $ tokenObject token
        LEFT_PAREN -> do
            exprMaybe <- expression
            case exprMaybe of
                Left err -> return $ Left err
                Right expr -> do 
                    consume RIGHT_PAREN MismatchedParenthesesError
                    return $ Right $ GroupingExpr expr
        IDENTIFIER -> return $ Right $ VariableExpr token
        _ -> return $ Left ExpectedExpressionError

matchTail :: [TokenType] -> State ParserState (Either ParserError Expr) -> State ParserState (Either ParserError (Maybe (Token, Expr)))
matchTail tokenTypes f = do
    maybeOperator <- matchToken tokenTypes
    case maybeOperator of
        Nothing -> return $ Right Nothing
        Just op -> do
            exprMaybe <- comparison
            restMaybe <- matchTail tokenTypes f
            case (exprMaybe, restMaybe) of
                (Left err, _) -> return $ Left err
                (_, Left err) -> return $ Left err
                (Right expr, Right rest) -> return $ Right $ Just (op, mergeExpressionMaybe expr rest)

mergeExpressionMaybe :: Expr -> Maybe (Token, Expr) -> Expr
mergeExpressionMaybe expr Nothing = expr
mergeExpressionMaybe left (Just (op, right)) = BinaryExpr left op right

matchToken :: [TokenType] -> State ParserState (Maybe Token)
matchToken [] = return Nothing
matchToken (t:ts) = do
    isMatch <- check t
    if isMatch then Just <$> advance else matchToken ts

check :: TokenType -> State ParserState Bool
check t = do
    atEnd <- isAtEnd
    if atEnd then return False else (== t) . tokenType <$> peek

consume :: TokenType -> ParserError -> State ParserState (Either ParserError Token)
consume t err = do
    isOk <- check t
    if isOk then Right <$> advance else return $ Left err

advance :: State ParserState Token
advance = state (\s@ParserState {tokens=(t:ts)} -> (t, s {tokens = ts}))

peek :: State ParserState Token
peek = gets (head . tokens) 

isAtEnd :: State ParserState Bool
isAtEnd = (== EOF) . tokenType <$> peek
