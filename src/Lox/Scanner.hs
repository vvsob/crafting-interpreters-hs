module Lox.Scanner where

import Control.Monad.State.Lazy
import Control.Monad.Extra

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
    deriving Show

-- undefined for now
data Object = NullObject deriving Show

data Token = Token {
    getType :: TokenType, 
    getLexeme :: String,
    getObject :: Object,
    getLineNumber :: Int
} deriving Show

data ScannerState = ScannerState {source :: String, tokens :: [Token]}

emptyScannerState :: String -> ScannerState
emptyScannerState source = ScannerState {source=source, tokens=[]}

scanTokensFromSource :: String -> [Token]
scanTokensFromSource source = evalState scanTokens (emptyScannerState source)

scanTokens :: State ScannerState [Token]
scanTokens = whileM (scanToken >> (not <$> isAtEnd)) >> gets (reverse . tokens)

isAtEnd :: State ScannerState Bool
isAtEnd = gets scannerIsAtEnd

scannerIsAtEnd :: ScannerState -> Bool
scannerIsAtEnd ScannerState {source=source, tokens=_} = null source

scanToken :: State ScannerState ()
scanToken = do
    c <- advance
    token <- case c of
        '(' -> addToken LEFT_PAREN
        ')' -> addToken RIGHT_PAREN
        '{' -> addToken LEFT_BRACE
        '}' -> addToken RIGHT_BRACE
        ',' -> addToken COMMA
        '.' -> addToken DOT
        '-' -> addToken MINUS
        '+' -> addToken PLUS
        ';' -> addToken SEMICOLON
        '*' -> addToken STAR
        '!' -> ifM (match '=') (addToken BANG_EQUAL) (addToken BANG)
        '=' -> ifM (match '=') (addToken EQUAL_EQUAL) (addToken EQUAL)
        '<' -> ifM (match '=') (addToken LESS_EQUAL) (addToken LESS)
        '>' -> ifM (match '=') (addToken GREATER_EQUAL) (addToken GREATER)
        _ -> error "Lexical error" -- TODO error handling
    return ()

advance :: State ScannerState Char
advance = state scannerAdvance

scannerAdvance :: ScannerState -> (Char, ScannerState)
scannerAdvance ScannerState {source=(c:cs), tokens=tokens} = (c, ScannerState {source=cs, tokens=tokens})

match :: Char -> State ScannerState Bool
match c = state (scannerMatch c)

scannerMatch :: Char -> ScannerState -> (Bool, ScannerState)
scannerMatch matchChar ScannerState {source=(sourceChar:sourceTail), tokens=tokens} = (matchChar == sourceChar, ScannerState {source=source, tokens=tokens})
    where source = if matchChar == sourceChar then sourceTail else sourceChar : sourceTail
scannerMatch _ state@ScannerState {source="", tokens=_} = (False, state)

peek :: State ScannerState Char
peek = gets $ head . source

addToken :: TokenType -> State ScannerState ()
addToken token = modify $ scannerAddLiteralToken token NullObject

addLiteralToken :: TokenType -> Object -> State ScannerState ()
addLiteralToken token object = modify $ scannerAddLiteralToken token object 

scannerAddLiteralToken :: TokenType -> Object -> ScannerState -> ScannerState
scannerAddLiteralToken tokenType object ScannerState {source=source, tokens=tokens} = ScannerState {source=source, tokens=token : tokens}
    where token = Token {getType=tokenType, getLexeme="", getObject=object, getLineNumber= -1}

