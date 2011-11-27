{-# LANGUAGE TemplateHaskell #-}
import TH
import Data.List

import System.Environment

-- Adapted from this blog post: 
--      http://neilmitchell.blogspot.com/2011/10/template-haskell-fights-with-generic.html
-- See also:
--      http://haskell.org/ghc/docs/7.2.2/html/users_guide/template-haskell.html
--      http://www.haskell.org/haskellwiki/Template_Haskell
--      http://www.haskell.org/haskellwiki/Uniplate

-- Things to try in ghci:
--      runQ [d| foo x = x + 1 |]
--      runQ [d| foo x = x + 1 |] >>= print
--      $(stringE . show =<< reify ''Int)

$(iHateDelete
    [d|
        mapDelete x = map (delete x)
        myElem x xs = length (delete x xs) /= length xs
    |])

main :: IO ()
main = do 
        args <- getArgs
        print $ show args
