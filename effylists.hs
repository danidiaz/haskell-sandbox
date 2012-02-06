module Main where

import System.Environment
import System.IO
--import Data.List
import Data.Maybe
import Control.Monad.ListT
import Data.List.Class
import qualified Data.List.Class as L
import Data.Monoid
import Control.Monad.IO.Class

prompt::IO String
prompt= putStrLn "write a line: " >> getLine 

effyl:: ListT IO String 
--effyl= return "foo"
--effyl= ListT $ (return getLine) >>= (\x -> Cons x mempty) 
effyl= (liftIO prompt) `mappend` (liftIO prompt) `mappend` effyl

effyl2:: ListT IO String 
effyl2= "foo" `L.cons` mempty

--effyl= (liftIO getLine) `L.cons` (liftIO getLine) `L.cons` effyl
main :: IO ()
main = do 
        args <- getArgs
        print $ "foo"
