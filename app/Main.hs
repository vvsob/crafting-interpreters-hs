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
            let exprMaybe = parse tokens
            case exprMaybe of
                Left ExpectedExpressionError -> putStrLn "Expected expression"
                Left MismatchedParenthesesError -> putStrLn "Mismatched parentheses"
                Right expr -> do
                    result <- eval expr
                    print result

main :: IO ()
main = putStr ">> " >> hFlush stdout >> getLine >>= run 
