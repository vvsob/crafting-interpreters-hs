module Lox.Parser (
    SyntaxError (..),
    parse
) where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.Maybe
import Lox.Scanner
import Lox.Expr
import Control.Monad.Extra (ifM)

data ParserState = ParserState {tokens :: [Token]}

data SyntaxError = SyntaxError String deriving Show

-- program        → declaration* EOF ;
--
-- declaration    → varDecl
--                | statement ;
--
-- statement      → exprStmt
--                | ifStmt
--                | printStmt 
--                | block ;
--
-- exprStmt       → expression ";" ;
-- ifStmt         → "if" "(" expression ")" statement 
--                ( "else" statement )? ;
-- printStmt      → "print" expression ";" ;
-- varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
-- block          → "{" declaration* "}" ;
--
-- expression     → assignment ;
-- assignment     → IDENTIFIER "=" assignment 
--                | logic_or;
-- logic_or       → logic_and ( "or" logic_and )* ;
-- logic_and      → equality ( "and" equality )* ;
-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- term           → factor ( ( "-" | "+" ) factor )* ;
-- factor         → unary ( ( "/" | "*" ) unary )* ;
-- unary          → ( "!" | "-" ) unary
--                | primary ;
-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--                | "(" expression ")" | IDENTIFIER;

parse :: [Token] -> Either SyntaxError [Stmt]
parse tokens = evalState program (ParserState {tokens=tokens})

program :: State ParserState (Either SyntaxError [Stmt])
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
declaration :: State ParserState (Either SyntaxError Stmt)
declaration = do
    varMaybe <- matchToken [VAR]
    case varMaybe of
        Just _ -> varDeclaration
        _ -> statement

varDeclaration :: State ParserState (Either SyntaxError Stmt)
varDeclaration = do
    maybeName <- consume IDENTIFIER $ SyntaxError "Expected variable name" 
    case maybeName of
        Left err -> return $ Left err
        Right name -> do
            hasInit <- isJust <$> matchToken [EQUAL]
            initMaybe <- if hasInit then expression else return $ Right $ LiteralExpr NullObject
            semicolonMaybe <- consume SEMICOLON $ SyntaxError "Expected semicolon" 
            case (initMaybe, semicolonMaybe) of
                (Left err, _) -> return $ Left err
                (_, Left err) -> return $ Left err
                (Right init, Right _) -> return $ Right $ VariableStmt name init


statement :: State ParserState (Either SyntaxError Stmt)
statement = do
    tokenMaybe <- matchToken [IF, PRINT, LEFT_BRACE]
    case tokenMaybe of
        Just (Token {tokenType=IF}) -> ifStatement
        Just (Token {tokenType=PRINT}) -> printStatement
        Just (Token {tokenType=LEFT_BRACE}) -> do 
            result <- fmap BlockStmt <$> block
            braceMaybe <- consume RIGHT_BRACE $ SyntaxError "Expected '}' after block"
            return $ braceMaybe >> result
        _ -> expressionStatement

block :: State ParserState (Either SyntaxError [Stmt])
block = do
    isRightBrace <- check RIGHT_BRACE
    if isRightBrace then return $ Right [] else do
        declMaybe <- declaration
        tailMaybe <- block
        case (declMaybe, tailMaybe) of
            (Left err, _) -> return $ Left err
            (_, Left err) -> return $ Left err
            (Right decl, Right tail) -> return $ Right $ decl : tail

ifStatement :: State ParserState (Either SyntaxError Stmt)
ifStatement = do
    leftParenMaybe <- consume LEFT_PAREN $ SyntaxError "Expected '(' after 'if'"
    conditionMaybe <- expression
    rightParenMaybe <- consume RIGHT_PAREN $ SyntaxError "Expected ')' after if condition"
    thenBranchMaybe <- statement
    isElse <- isJust <$> matchToken [ELSE]
    elseBranchMaybe <- if isElse then fmap Just <$> statement else return $ Right Nothing
    return $ IfStmt <$> (leftParenMaybe >> conditionMaybe <* rightParenMaybe) <*> thenBranchMaybe <*> elseBranchMaybe

printStatement :: State ParserState (Either SyntaxError Stmt)
printStatement = do
    valueMaybe <- expression
    semicolonMaybe <- consume SEMICOLON $ SyntaxError "Expected ';'"
    case (valueMaybe, semicolonMaybe) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right value, Right _) -> return $ Right $ PrintStmt value

expressionStatement :: State ParserState (Either SyntaxError Stmt)
expressionStatement = do
    valueMaybe <- expression
    semicolonMaybe <- consume SEMICOLON $ SyntaxError "Expected ';'"
    case (valueMaybe, semicolonMaybe) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right value, Right _) -> return $ Right $ ExpressionStmt value
    

expression :: State ParserState (Either SyntaxError Expr)
expression = assignment

assignment :: State ParserState (Either SyntaxError Expr)
assignment = do
    maybeExpr <- logicalOr
    matchedEqual <- isJust <$> matchToken [EQUAL]
    if matchedEqual then do
        maybeValue <- assignment
        case (maybeExpr, maybeValue) of
            (Left err, _) -> return $ Left err
            (_, Left err) -> return $ Left err
            (Right (VariableExpr name), Right value) -> return $ Right $ AssignmentExpr name value
            _ -> return $ Left $ SyntaxError "Invalid assignment target" 
    else return maybeExpr

logicalOr :: State ParserState (Either SyntaxError Expr)
logicalOr = do
    exprMaybe <- logicalAnd
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionLogicalMaybe expr) <$> matchTailLogical [OR] logicalAnd

logicalAnd :: State ParserState (Either SyntaxError Expr)
logicalAnd = do
    exprMaybe <- equality
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionLogicalMaybe expr) <$> matchTailLogical [AND] logicalAnd

equality :: State ParserState (Either SyntaxError Expr)
equality = do
    exprMaybe <- comparison
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionMaybe expr) <$> matchTail [BANG_EQUAL, EQUAL_EQUAL] comparison

comparison :: State ParserState (Either SyntaxError Expr)
comparison = do
    exprMaybe <- term 
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionMaybe expr) <$> matchTail [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] term

term :: State ParserState (Either SyntaxError Expr)
term = do
    exprMaybe <- factor
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionMaybe expr) <$> matchTail [MINUS, PLUS] factor

factor :: State ParserState (Either SyntaxError Expr)
factor = do
    exprMaybe <- unary
    case exprMaybe of
        Left err -> return $ Left err
        Right expr -> fmap (mergeExpressionMaybe expr) <$> matchTail [SLASH, STAR] unary

unary :: State ParserState (Either SyntaxError Expr)
unary = do
    maybeOperator <- matchToken [BANG, MINUS]
    case maybeOperator of
        Nothing -> primary
        Just op -> do
            exprMaybe <- unary
            case exprMaybe of
                Left err -> return $ Left err
                Right expr -> return $ Right $ UnaryExpr op expr

primary :: State ParserState (Either SyntaxError Expr)
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
                    consume RIGHT_PAREN $ SyntaxError "Mismatched parentheses"
                    return $ Right $ GroupingExpr expr
        IDENTIFIER -> return $ Right $ VariableExpr token
        _ -> return $ Left $ SyntaxError "Expected expression" 

matchTail :: [TokenType] -> State ParserState (Either SyntaxError Expr) -> State ParserState (Either SyntaxError (Maybe (Token, Expr)))
matchTail tokenTypes = matchTailWith tokenTypes mergeExpressionMaybe

matchTailLogical :: [TokenType] -> State ParserState (Either SyntaxError Expr) -> State ParserState (Either SyntaxError (Maybe (Token, Expr)))
matchTailLogical tokenTypes = matchTailWith tokenTypes mergeExpressionLogicalMaybe

matchTailWith :: [TokenType] -> (Expr -> Maybe (Token, Expr) -> Expr) -> State ParserState (Either SyntaxError Expr) -> State ParserState (Either SyntaxError (Maybe (Token, Expr)))
matchTailWith tokenTypes m f = do
    maybeOperator <- matchToken tokenTypes
    case maybeOperator of
        Nothing -> return $ Right Nothing
        Just op -> do
            exprMaybe <- comparison
            restMaybe <- matchTail tokenTypes f
            case (exprMaybe, restMaybe) of
                (Left err, _) -> return $ Left err
                (_, Left err) -> return $ Left err
                (Right expr, Right rest) -> return $ Right $ Just (op, m expr rest)


mergeExpressionMaybe :: Expr -> Maybe (Token, Expr) -> Expr
mergeExpressionMaybe expr Nothing = expr
mergeExpressionMaybe left (Just (op, right)) = BinaryExpr left op right

mergeExpressionLogicalMaybe :: Expr -> Maybe (Token, Expr) -> Expr
mergeExpressionLogicalMaybe expr Nothing = expr
mergeExpressionLogicalMaybe left (Just (op, right)) = LogicalExpr left op right

matchToken :: [TokenType] -> State ParserState (Maybe Token)
matchToken [] = return Nothing
matchToken (t:ts) = do
    isMatch <- check t
    if isMatch then Just <$> advance else matchToken ts

check :: TokenType -> State ParserState Bool
check t = do
    atEnd <- isAtEnd
    if atEnd then return False else (== t) . tokenType <$> peek

consume :: TokenType -> SyntaxError -> State ParserState (Either SyntaxError Token)
consume t err = do
    isOk <- check t
    if isOk then Right <$> advance else return $ Left err

advance :: State ParserState Token
advance = state (\s@ParserState {tokens=(t:ts)} -> (t, s {tokens = ts}))

peek :: State ParserState Token
peek = gets (head . tokens) 

isAtEnd :: State ParserState Bool
isAtEnd = (== EOF) . tokenType <$> peek
