import Lox.Scanner
import Lox.Parser
import Lox.Interpreter
import System.IO
import System.Environment

run :: String -> IO ()
run source = do 
    let tokensMaybe = scanTokensFromSource source
    case tokensMaybe of
        Left UnexpectedCharacterError -> putStrLn "Unexpected character"
        Right tokens -> do
            let stmtMaybe = parse tokens
            case stmtMaybe of
                Left (SyntaxError s) -> putStrLn s
                Right statements -> runStatements statements

runEval :: String -> IO ()
runEval source = do 
    let tokensMaybe = scanTokensFromSource source
    object <- case tokensMaybe of
        Left UnexpectedCharacterError -> putStrLn "Unexpected character" >> return NullObject
        Right tokens -> do
            let exprMaybe = parseExpression tokens
            case exprMaybe of
                Left (SyntaxError s) -> putStrLn s >> return NullObject
                Right statements -> eval statements
    print object

repl :: IO ()
repl = putStr ">> " >> hFlush stdout >> getLine >>= runEval

main :: IO ()
main = getArgs >>= fs

fs :: [String] -> IO ()
fs [] = repl
fs [s] = readFile s >>= run
fs _ = putStrLn "Usage: lox [file]"
