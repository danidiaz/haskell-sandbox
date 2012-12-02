module Main (main) where

import           System.Console.GetOpt
import           System.IO
import           System.Environment
import           System.Exit
import           Control.Monad.Logic
import           Control.Monad.Logic.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO


foo :: Logic (Maybe Int) -> Logic Char
foo x = do
    Just i <- x
    if i>3 then
        return 'a'
    else 
        return 'b'

ence x = do Just (a,_) <- msplit x
            return a

main :: IO ()
main = 
    do
        putStrLn $ show $ observe $ foo $ return (Nothing)
        return ()
