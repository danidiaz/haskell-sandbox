{-# LANGUAGE ScopedTypeVariables #-}  
import Control.Monad
import Control.Monad.Error
import Data.Either
import Control.Monad.Trans.Control (control,liftBaseOp)
import Control.Exception
import System.IO
import Prelude hiding (catch)


data FibException = FileUnreadable IOError
                  | FormatError String String
                  | InvalidValue Integer
                  | Unknown String

instance Error FibException where
    noMsg = Unknown "No error message"
    strMsg = Unknown
    
type FibIOMonad = ErrorT FibException IO

catchError' :: ErrorT e IO a -> (IOError -> ErrorT e IO a) -> ErrorT e IO a
catchError' m f =
    ErrorT $ catchError (runErrorT m) (fmap runErrorT f)   
    
verifyFibFile :: FilePath -> FibIOMonad ()
verifyFibFile path = do
    sequenceStr <- catchError' (liftIO $ readFile path) (throwError . FileUnreadable)
    ErrorT . return $ Right ()

verifyFibFile'' :: FilePath -> FibIOMonad ()
verifyFibFile'' path = do
    sequenceStr <- control $ \run -> catch (run . liftIO $ readFile path) 
                                           (run . throwError . FileUnreadable)     
    ErrorT . return $ Right ()


-- Turning IO exceptions into monadic errors using monad-control.
-- See http://www.yesodweb.com/blog/2011/08/monad-control.
-- http://blog.ezyang.com/2012/01/monadbasecontrol-is-unsound/
-- http://blog.ezyang.com/2012/01/modelling-io/
foo:: ErrorT String IO String
foo = control $ \run ->
    catch
         (run $ liftIO $ readFile "foo.txt")
         (\(e::IOException) -> run (throwError "couldn't open") )

--
foo2:: ErrorT String IO String -> ErrorT String IO String
foo2 action = control $ \run ->
    catch
         (run action)
         (\(e::IOException) -> run (throwError "couldn't open") )

main :: IO ()
main = do
    result <- runErrorT foo
    putStrLn . show $ result
    -- What happens if a received action is already a monadic error?
    result2 <- runErrorT . foo2 . throwError $ "preexistent error"
    putStrLn . show $ result2
