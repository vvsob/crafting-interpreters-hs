module Main where

import Lox.Scanner

run :: String -> IO ()
run source = mapM_ print tokens
    where tokens = scanTokensFromSource source

main :: IO ()
main = getLine >>= run 
