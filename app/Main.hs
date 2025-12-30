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

repl :: IO ()
repl = putStr ">> " >> hFlush stdout >> getLine >>= run 

main :: IO ()
main = getArgs >>= fs

fs :: [String] -> IO ()
fs [] = repl
fs [s] = readFile s >>= run
fs _ = putStrLn "Usage: lox [file]"
