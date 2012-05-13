module Main where

import System.Environment
import Data.List
import Data.Maybe
import qualified Data.Map as M
    
-- Experiments with list comprehensions and records.

data Component = Component
    {
        _hash::Int,
        _capabilities::[Foo]
    } deriving Show

data Foo =
    A Int
    |B String   
    |C Int
    deriving Show

clist::[Component]
clist = [
       Component 0 [A 4, B "foo"],
       Component 1 [B "ee"],
       Component 2 [C 7],
       Component 3 [A 8, B "dd"],
       Component 4 [A 4, B "gg"]
    ]
   
-- Get all B values for which the component also has A = 4
filtered :: [String]
filtered = do
   c <- clist
   A 4  <- _capabilities c
   B z <- _capabilities c
   return z
       
main = do 
    putStrLn . show $ filtered
    putStrLn . show $ listToMaybe filtered

