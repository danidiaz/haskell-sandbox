module Main (main) where

import           System.Console.GetOpt
import           System.IO
import           System.Environment
import           System.Exit
import qualified System.IO as SIO

import           Data.ByteString
import           Data.Word

import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IOH
import           Data.Iteratee.Base.ReadableChunk
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
       -- Enumerating a handle over a finished iteratee doesn't read
       -- from the handle, and doesn't throw an exception
       handle <- openFile "iterdata/source.txt" ReadMode
       let  
            finishediter = I.idone () (I.EOF Nothing::I.Stream [Word8])
       i <- IOH.enumHandle 22 handle finishediter >>= I.run
       str <- SIO.hGetLine handle
       SIO.putStr str
       SIO.hClose handle
