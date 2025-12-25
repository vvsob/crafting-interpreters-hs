module Lox.Scanner (
    TokenType (..),
    Object (..),
    Token (getType, getLexeme, getObject),
    scanTokensFromSource
) where

import Control.Monad.State.Lazy
import Control.Monad.Extra
import Data.Char
import Data.Maybe

data TokenType = LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE 
               | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR
               | BANG | BANG_EQUAL
               | EQUAL | EQUAL_EQUAL
               | GREATER | GREATER_EQUAL
               | LESS | LESS_EQUAL
               | IDENTIFIER | STRING | NUMBER
               | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR
               | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE
               | EOF
    deriving (Show, Eq)

data Object = NullObject | StringObject String | NumberObject Double | BoolObject Bool deriving Show

data Token = Token {
    getType :: TokenType, 
    getLexeme :: String,
    getObject :: Object,
    getLineNumber :: Int
} deriving Show

data ScannerState = ScannerState {source :: String, current :: String, lineNumber :: Int}

emptyScannerState :: String -> ScannerState
emptyScannerState source = 
    ScannerState {source=source, current="", lineNumber=1}

scanTokensFromSource :: String -> [Token]
scanTokensFromSource source = evalState scanTokens (emptyScannerState source)

scanTokens :: State ScannerState [Token]
scanTokens = do
    atEnd <- isAtEnd
    if atEnd then return [] else do
        maybeToken <- scanToken
        case maybeToken of
            Nothing -> scanTokens
            Just t -> (t :) <$> scanTokens

isAtEnd :: State ScannerState Bool
isAtEnd = gets scannerIsAtEnd

scannerIsAtEnd :: ScannerState -> Bool
scannerIsAtEnd ScannerState {source=source} = null source

scanToken :: State ScannerState (Maybe Token)
scanToken = do
    resetCurrent
    c <- advance
    case c of
        '(' -> Just <$> addToken LEFT_PAREN
        ')' -> Just <$> addToken RIGHT_PAREN
        '{' -> Just <$> addToken LEFT_BRACE
        '}' -> Just <$> addToken RIGHT_BRACE
        ',' -> Just <$> addToken COMMA
        '.' -> Just <$> addToken DOT
        '-' -> Just <$> addToken MINUS
        '+' -> Just <$> addToken PLUS
        ';' -> Just <$> addToken SEMICOLON
        '*' -> Just <$> addToken STAR
        '!' -> Just <$> ifM (match '=') (addToken BANG_EQUAL) (addToken BANG)
        '=' -> Just <$> ifM (match '=') (addToken EQUAL_EQUAL) (addToken EQUAL)
        '<' -> Just <$> ifM (match '=') (addToken LESS_EQUAL) (addToken LESS)
        '>' -> Just <$> ifM (match '=') (addToken GREATER_EQUAL) (addToken GREATER)
        '/' -> ifM (match '/') (advanceLine >> return Nothing) (Just <$> addToken SLASH)
        '"' -> Just <$> scanString
        ' ' -> return Nothing
        '\r' -> return Nothing
        '\t' -> return Nothing
        '\n' -> return Nothing
        c -> if isDigit c then Just <$> scanNumber else if isAlpha c then Just <$> scanIdentifier else error "Unexpected character"

scanString :: State ScannerState Token
scanString = do
    whileM (do
        c <- peek
        atEnd <- isAtEnd
        unless (c == '"' || atEnd) (do 
            when (c == '\n') (modify (\s -> s {lineNumber=lineNumber s + 1}))
            advance
            return ())
        return $ not (c == '"' || atEnd))
    advance
    value <- gets (init . tail . current)
    addLiteralToken STRING (StringObject value)

scanNumber :: State ScannerState Token
scanNumber = do
    advanceWhile isDigit
    isFraction <- ((&&) . (== '.') <$> peek) <*> (isDigit <$> peekNext)
    when isFraction (advance >> advanceWhile isDigit)
    value <- gets (read . current)
    addLiteralToken NUMBER (NumberObject value)

advanceWhile:: (Char -> Bool) -> State ScannerState ()
advanceWhile pred = do
    c <- peek
    when (pred c) (advance >> advanceWhile pred)

scanIdentifier :: State ScannerState Token
scanIdentifier = do
    advanceWhile isAlphaNum
    value <- gets current
    let tokenType = getKeywordTokenType value
    addToken tokenType

resetCurrent :: State ScannerState ()
resetCurrent = modify (\state -> state {current=""})

advance :: State ScannerState Char
advance = state scannerAdvance

scannerAdvance :: ScannerState -> (Char, ScannerState)
scannerAdvance state@ScannerState {source=(c:cs), current=current} = 
    (c, state {source=cs, current=current ++ [c]})
scannerAdvance state@ScannerState {source=""} = ('\0', state)

advanceLine :: State ScannerState ()
advanceLine = do
    c <- advance
    atEnd <- isAtEnd
    unless (c == '\n' || atEnd) advanceLine

match :: Char -> State ScannerState Bool
match c = state (scannerMatch c)

scannerMatch :: Char -> ScannerState -> (Bool, ScannerState)
scannerMatch matchChar state@ScannerState {source=(sourceChar:sourceTail), current=current} =
    (matchChar == sourceChar, state {source=source, current=newCurrent})
    where source = if matchChar == sourceChar then sourceTail else sourceChar : sourceTail
          newCurrent = if matchChar == sourceChar then current ++ [sourceChar] else current
scannerMatch _ state@ScannerState {source=""} = (False, state)

peek :: State ScannerState Char
peek = gets (\s -> if null $ source s then '\0' else head $ source s)

peekNext :: State ScannerState Char
peekNext = gets (\s -> if null (source s) || null ( tail $ source s) then '\0' else head $ tail $ source s)

addToken :: TokenType -> State ScannerState Token
addToken token = state $ scannerAddLiteralToken token NullObject

addLiteralToken :: TokenType -> Object -> State ScannerState Token
addLiteralToken token object = state $ scannerAddLiteralToken token object 

scannerAddLiteralToken :: TokenType -> Object -> ScannerState -> (Token, ScannerState)
scannerAddLiteralToken tokenType object state@ScannerState {current=current, lineNumber=lineNumber} =
    (token, state)
    where token = Token {getType=tokenType, getLexeme=current, getObject=object, getLineNumber=lineNumber}

getKeywordTokenType :: String -> TokenType
getKeywordTokenType "and" = AND
getKeywordTokenType "class" = CLASS
getKeywordTokenType "else" = ELSE
getKeywordTokenType "false" = FALSE
getKeywordTokenType "for" = FOR
getKeywordTokenType "fun" = FUN
getKeywordTokenType "if" = IF
getKeywordTokenType "nil" = NIL
getKeywordTokenType "or" = OR
getKeywordTokenType "print" = PRINT
getKeywordTokenType "return" = RETURN
getKeywordTokenType "super" = SUPER
getKeywordTokenType "this" = THIS
getKeywordTokenType "true" = TRUE
getKeywordTokenType "var" = VAR
getKeywordTokenType "while" = WHILE
getKeywordTokenType _ = IDENTIFIER

