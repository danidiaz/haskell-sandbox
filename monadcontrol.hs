{-# LANGUAGE ScopedTypeVariables #-}  
module Main (main) where
import Control.Monad
import Control.Monad.Error
import Data.Either
import Control.Monad.Trans.Control
import Control.Exception
import System.IO
import Prelude hiding (catch)

-- Turning IO exceptions into ErrorT errors using monad-control.
-- See http://www.yesodweb.com/blog/2011/08/monad-control.
foo:: ErrorT String IO String
foo = control $ \run ->
    catch
         (run $ liftIO $ readFile "foo.txt")
         (\(e::IOException) -> run (throwError "couldn't open") )

main :: IO ()
main = do
    result <- runErrorT foo
    putStrLn . show $ result
    
