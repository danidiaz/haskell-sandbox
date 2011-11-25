module Main (main) where

import           System.Console.GetOpt
import           System.IO
import           System.Environment
import           System.Exit

import qualified Data.Iteratee as I

import           Data.Iteratee ((><>),(<><))
import           Data.Iteratee.Char
import qualified Data.Iteratee.Char as IC

main :: IO ()
main = 
    do let 
            enum = I.enumPure1Chunk [1..1000::Int]
            it = (I.joinI $ (I.take 15 ><> I.take 10) I.stream2list)
       rs <- enum it >>= I.run
       print rs
