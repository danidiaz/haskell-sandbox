module Main (main) where

import           System.Console.GetOpt
import           System.IO
import           System.Environment
import           System.Exit
import qualified System.IO as SIO
import           Control.Exception  
import           Data.ByteString
import           Data.Word
import           Control.Monad.IO.Class
import           Data.ListLike.CharString
import qualified Data.Iteratee as I
import qualified Data.Iteratee.ListLike as IL
import qualified Data.Iteratee.IO.Handle as IOH
import           Data.Iteratee.Base.ReadableChunk
import           Data.Iteratee ((><>),(<><))
import           Data.Iteratee.Char
import qualified Data.Iteratee.Char as IC

main :: IO ()
main = 
    do 
       handle <- openFile "iterdata/source.txt" ReadMode
       let  
            enum = IL.take 10
            it1 = I.joinI $ enum (I.stream2list::I.Iteratee ByteString IO [Word8])
            it3 = do
                l1 <- it1
                r <- liftIO $ try (openFile "doesnotexist" ReadMode) 
                case r of
                    Left e -> liftIO . SIO.putStrLn . show $ (e::SomeException)
                    Right _ -> liftIO . SIO.putStrLn $ "exists" 
                return l1
       i <- IOH.enumHandle 7 handle it3 >>= I.run
       SIO.putStrLn $ show i
       SIO.hClose handle
