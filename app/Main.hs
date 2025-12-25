import Lox.Scanner
import Lox.Parser
import Lox.Interpreter

run :: String -> IO ()
run source = print result
    where result = eval expr
          expr = parse tokens
          tokens = scanTokensFromSource source

main :: IO ()
main = getLine >>= run 
