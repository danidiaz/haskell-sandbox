{-# LANGUAGE DeriveDataTypeable #-} 

module ErrorHandlingExamples (
        errorHandlingExample1,
        errorHandlingExample2
) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import qualified Control.Exception as CE
import qualified Control.Exception.Control as CEC
import Data.Typeable

-- Run with: runIdentity $ runErrorT $ errorHandlingExample1 7 1      
errorHandlingExample1:: (Fractional a) => a -> a -> ErrorT String Identity a
errorHandlingExample1 nume deno = do
        x <- catchError operation handler
        return $ (+) x 1
        where 
                operation = if deno==0 
                            then throwError "Division by zero!"
                            else return $ nume/deno 
                handler = (\_->return 77)


data FooException = FooException
     deriving (Show, Typeable)

instance CE.Exception FooException

-- Catching IO exceptions inside ErrorT.
-- Naive attempt that DOESN'T WORK. 
-- The FooException passes through.
-- Run with: runErrorT errorHandlingExample2
errorHandlingExample2:: ErrorT String IO Int
errorHandlingExample2 = 
     let handler = \_ -> throwError "oops..." 
     in do
           catchError (liftIO $ CE.throwIO FooException) handler
           return 5

-- Catching IO exceptions inside ErrorT.
-- Uses monad-control, and it works.
-- See http://www.yesodweb.com/blog/2011/08/monad-control
-- Run with: runErrorT errorHandlingExample3
errorHandlingExample3:: ErrorT String IO Int
errorHandlingExample3 =
     let 
        handler::FooException -> ErrorT String IO Int 
        handler = \_ -> throwError "oops..." 
     in do
           CEC.catch (CEC.throwIO FooException) handler 
           return 5     


