module Lox.Environment (
    Environment (..),
    emptyEnvironment,
    define,
    get,
    assign
) where

import Data.Map
import Lox.Scanner

data Environment = Environment {enclosing :: Maybe Environment, variables :: Map String Object}

emptyEnvironment :: Environment
emptyEnvironment = Environment {enclosing=Nothing, variables=empty}

define :: String -> Object -> Environment -> Environment
define key value env@Environment {variables=variables} = env {variables=insert key value variables}

get :: String -> Environment -> Maybe Object
get key Environment {enclosing=enclosing, variables=variables} = 
    case variables !? key of
        Just val -> Just val
        Nothing -> case enclosing of
            Just e -> get key e
            Nothing -> Nothing

assign :: String -> Object -> Environment -> (Bool, Environment)
assign key value env@Environment {enclosing=enclosing, variables=variables} = 
    if member key variables 
        then (True, env {variables=insert key value variables}) 
        else case enclosing of 
            Just e -> let (success, newEnclosing) = assign key value e in (success, env {enclosing = Just newEnclosing})
            Nothing -> (False, env)
