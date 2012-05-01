module Main where

import System.Environment
import Data.List
import Data.Maybe
import qualified Data.Map as M
    
data Component = Component
    {
        _hash::Int,
        _name::String,
        _dim::(Int,Int),
        _specific::Component',
        _children::[Component]
    } deriving Show

data Component' =
    Window String
    |Button
    |Panel
    |Other
    deriving Show

clist::[Component]
clist = [
       Component 0 "foo" (1,2) (Window "nana")  [
                Component 1 "nah" (3,50) Panel [] 
            ]     
    ]
   
titles = [ title | Component { _specific = Window title } <- clist]
-- this as-pattern does not require parentheses
titles' = [ (whole,title) | whole@Component { _specific = Window title } <- clist]
-- the inner as-pattern does require parentheses
titles'' = [ (c,c') | c@Component { _specific = c'@(Window title) } <- clist]
-- the {} in Window can be used even if Window is not a register 
titles''' = [ (c,c') | c@Component { _specific = c'@(Window {}) } <- clist]

main = do 
    args <- getArgs
    putStrLn . show $ args
    putStrLn . show $ clist
    putStrLn . show $ titles
    putStrLn . show $ titles''
    putStrLn . show $ titles'''
