module Main where

import System.Environment
import Data.List
import Data.Maybe
import qualified Data.Map as M

type UntiedGraph c = [(c,[c])] 

data Node c = Node { nodeId::c, nodeEdges::[Node c] }
type Graph c = M.Map c (Node c)

tieGraph :: Ord c => UntiedGraph c -> Graph c 
tieGraph untied = 
        let tied = M.mapWithKey buildNode $ M.fromList untied
            buildNode k ns = Node k (catMaybes $ map removeIndirection ns)
            removeIndirection n = M.lookup n tied
        in  tied

followPath :: Node c -> [Int] -> [Node c]
followPath =
    let next node index = (nodeEdges node)!!index
    in  scanl next 

graphDesc = [('a',"abc"),('b',"cd"),('c',"db"),('d',"ba")]

main :: IO ()
main = do 
        args <- getArgs
        let
            path = map read args::[Int] 
            startNode = head $ M.elems $ tieGraph graphDesc 
            visited = followPath startNode path 
        print $ map nodeId $ visited
