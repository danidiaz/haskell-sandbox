module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Either
import Control.Monad.List
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Free

-- what the hell is this?
foo:: FreeT ((,) Int) (Reader String) Char
foo = return 'c' >> lift ask >> return 'd'


main :: IO ()
main = do 
        putStrLn ""
