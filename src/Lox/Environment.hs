module Lox.Environment (
    Environment,
    emptyEnvironment,
    define,
    get,
    assign
) where

import Data.Map
import Lox.Scanner

data Environment = Environment {variables :: Map String Object}

emptyEnvironment :: Environment
emptyEnvironment = Environment {variables=empty}

define :: String -> Object -> Environment -> Environment
define key value env@Environment {variables=variables} = env {variables=insert key value variables}

get :: String -> Environment -> Maybe Object
get key Environment {variables=variables} = variables !? key

assign :: String -> Object -> Environment -> (Bool, Environment)
assign key value env@Environment {variables=variables} = 
    if member key variables then (True, env {variables=insert key value variables}) else (False, env)
