import Lox.Scanner
import Lox.Parser
import Lox.Interpreter
import System.IO

run :: String -> IO ()
run source = print result
    where result = eval expr
          expr = parse tokens
          tokens = scanTokensFromSource source

main :: IO ()
main = putStr ">> " >> hFlush stdout >> getLine >>= run 
