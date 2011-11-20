module Main (main) where

import           System.Console.GetOpt
import           System.IO
import           System.Environment
import           System.Exit

import           Data.List
import qualified Data.Text as T
import           Data.Enumerator (($=),(=$),($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.List as EL

changechar :: Char -> Char -> Char -> Char 
changechar a b c 
    | c == a = b
    | otherwise = c

openText :: FilePath -> IO Handle
openText filename = do
       handle <- openFile filename ReadMode 
       hSetEncoding handle utf8
       hSetBuffering handle $ BlockBuffering Nothing
       return handle

interspersetee :: Monad m => ai -> E.Enumeratee ai ai m b
interspersetee x = EL.concatMap (\u -> u:[x])

newlineinterspersetee :: Monad m => E.Enumeratee T.Text T.Text m b
newlineinterspersetee = interspersetee $ T.singleton '\n';

asintobs = ET.map $ changechar 'a' 'b'

main :: IO ()
main = 
    do
       args <- getArgs 
       handle <- openText $ head args
       res <- E.run $ 
                (ET.enumHandle handle) $= 
                newlineinterspersetee $= asintobs $$
--              asintobs $= newlineinterspersetee $$
--              ^^^ If asintobs is put *before* the newline iteratee, 
--                  a newline is added after each char. Odd!
                ET.iterHandle stdout
       hClose handle
