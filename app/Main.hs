module Main where

import Lox.Scanner

run :: String -> IO ()
run source = mapM_ print tokens
    where tokens = scanTokens source

main :: IO ()
main = putStrLn "Hello, Haskell!"
