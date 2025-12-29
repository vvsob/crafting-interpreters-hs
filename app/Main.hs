import Lox.Scanner
import Lox.Parser
import Lox.Interpreter
import System.IO

run :: String -> IO ()
run source = do 
    let tokensMaybe = scanTokensFromSource source
    case tokensMaybe of
        Left UnexpectedCharacterError -> putStrLn "Unexpected character"
        Right tokens -> do
            let stmtMaybe = parse tokens
            case stmtMaybe of
                Left ExpectedExpressionError -> putStrLn "Expected expression"
                Left MismatchedParenthesesError -> putStrLn "Mismatched parentheses"
                Left ExpectedSemicolonError -> putStrLn "Expected semicolon"
                Right statements -> runStatements statements

main :: IO ()
main = putStr ">> " >> hFlush stdout >> getLine >>= run 
