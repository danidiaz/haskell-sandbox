{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Prelude hiding (catch,(.),iterate,tail,repeat,sequence,take)
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Category
import Control.Monad hiding (sequence)
import Control.Comonad
import Control.Comonad.Trans.Class  
import Control.Comonad.Trans.Env
import System.Random
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Applicative
import Data.Stream.Infinite
import System.IO
import Debug.Trace
import qualified Data.Text as T

-- http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

data U x = U (Stream x) x (Stream x) deriving (Functor,Foldable,Traversable)

right (U a b (c:>cs)) = U (b:>a) c cs
left  (U (a:>as) b c) = U as a (b:>c)

instance Comonad U where
   extract (U _ b _) = b
   duplicate a = U (tail $ iterate left a) a (tail $ iterate right a)

type Probs = (Float,Float,Float,Float)

localRule :: EnvT Probs U Bool -> Rand StdGen Bool
localRule ca = 
    let (tt,tf,ft,ff) = ask ca
        black prob = (<prob) <$> getRandomR (0,1)
    in case lower ca of
        U (True:>_) _ (True:>_) -> black tt 
        U (True:>_) _ (False:>_) -> black tf 
        U (False:>_) _ (True:>_) -> black ft
        U (False:>_) _ (False:>_) -> black ff

evolve :: EnvT Probs U Bool -> Rand StdGen (EnvT Probs U Bool)
evolve ca = sequence $ extend localRule ca

history :: Int -> EnvT Probs U Bool -> Stream (EnvT Probs U Bool)
history seed initialca = 
    let unfoldf (ca,seed) = 
            let (seed',seed'') = runRand getSplit seed 
                nextca = evalRand (evolve ca) seed'
            in  (nextca,(nextca,seed''))
    in unfold unfoldf (initialca,mkStdGen seed)

showca :: Int -> U Bool -> String
showca margin ca = 
    let char b = if b then '#' else '_'
        U left center right = fmap char ca
    in (reverse $ take margin left) ++ [center] ++ (take margin right)

main :: IO ()
main = do
    let probs = (0.0,0.6,0.7,0.0)
        initialca = EnvT probs $ U (repeat False) True (repeat False) 
        seed = 77
        iterations = 10
        margin = 8
    hSetBuffering stdout NoBuffering
    -- putStrLn . showca margin . lower $ initialca 
    sequence . fmap (putStrLn . showca margin . lower) . take iterations $ history seed initialca
    return ()

