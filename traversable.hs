module Main where

import Data.Tree
import Data.List
import Data.String
import Data.Traversable
import qualified Data.Traversable as TRAV

indent :: String -> String
indent = unlines . map ((++) "\t") . lines 

pptree ::  Tree String -> IO ()
pptree t = do
        putStr "tree:\n"
        putStr . indent . drawTree $ t
        putStr "\n"

tree1 = Node "a" [Node "x" [], Node "y" []]
tree2 = Node "b" [Node "u" [], Node "v" []]

main :: IO ()
main = do 
        -- From the Typeclassopedia entry on Traversable:
        --      "A much more challenging question is whether a
        --      list of trees can be turned into a tree of lists."
        let t = sequenceA [tree1,tree2]
            tt = fmap concat t
        TRAV.mapM pptree [tree1,tree2,tt] 
        -- And now we try mapM over a tree, instead of over
        -- a list.
        TRAV.mapM putStr tree1
        return ()
        
