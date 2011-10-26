module ErrorHandlingExamples (
        errorHandlingExample1
) where

import Control.Monad
import Control.Monad.Error
-- import Control.Monad.Error.Class
import Control.Monad.Identity

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

