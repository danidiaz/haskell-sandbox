module MTExamples (
        errorIdentityExample,
        stateIOExample,
        errorIOExample,
        errorStateExample,
        stateErrorExample,
        errorStateIOExample 
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Either
import Control.Monad.List
import Control.Monad.Identity

-- Run with: runIdentity $ runErrorT $ errorIdentityExample 7 1      
errorIdentityExample :: (Fractional a) => a -> a -> ErrorT String Identity a
errorIdentityExample nume deno = 
        if deno==0 then
                fail "Division by zero!"
        else return $ nume/deno 


-- Run with: runStateT stateIOExample "Initial state"
stateIOExample :: StateT String IO () 
stateIOExample = do
  liftIO $ putStr "write something, empty string to quit: "
  str   <- liftIO getLine
  state <- get
  liftIO $ putStrLn ("current state: " ++ state)
  case str of 
        _:_ -> do
          liftIO $ putStrLn ("new state: " ++ str)
          put str
          stateIOExample
        []  -> return ()


-- Run with: runErrorT errorIOExample 
errorIOExample :: ErrorT String IO String
errorIOExample = do
  liftIO $ putStr "write something, empty string will get an error: "
  str   <- liftIO getLine
  case str of 
        _:_ -> return str
        []  -> fail "empty string!"


-- Run with: runState (runErrorT errorStateExample) 7
errorStateExample :: ErrorT String (State Int) String
errorStateExample = do
        n <- lift get
        if n == 7 
        then do
            fail "I don't like the number 7!"
            return "This will never be returned"
        else do
            lift $ put (n+1)
            n2 <- lift get
            return $ show n2
                                     

-- Run with: runStateT stateErrorExample 7
stateErrorExample :: StateT Int (Either String) String 
stateErrorExample = do
       n <- get
       if n == 7 
       then do
           lift $ Left "I really don't like the number 7!"             
       else do
           put (n+1)
           n2 <- get
           return $ show n2


-- Run with: runStateT (runErrorT errorStateIOExample) 2
errorStateIOExample :: ErrorT String (StateT Int IO) String 
errorStateIOExample = do
        liftIO $ putStr "write something, empty string will get an error: "
        str   <- liftIO getLine
        state <- lift get
        liftIO $ putStrLn ("current state: " ++ show state)
        case str of 
                _:_ -> let newState = state + (length str) in 
                       do
                          liftIO $ putStrLn ("new state: " ++ (show newState))
                          lift $ put newState 
                          return str
                []  -> fail "No empty strings allowed!" 


