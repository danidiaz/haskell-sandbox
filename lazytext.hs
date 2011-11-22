module Main (main) where

import           System.Console.GetOpt
import           System.IO
import           System.Environment
import           System.Exit
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

tchunk :: Char -> Int -> T.Text
tchunk c n = T.replicate n $ T.singleton c

textchunklist :: [T.Text]
textchunklist =
    let go c = tchunk c 5 : (go $ succ c)
    in go 'a'

lazytext :: TL.Text
lazytext = TL.fromChunks textchunklist

main :: IO ()
main = 
    do
       TLIO.putStr $ TL.take 77 lazytext
