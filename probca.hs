{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

-- uses packages: comonad-transformers,streams,MonadRandom

import Prelude hiding (iterate,tail,repeat,sequence,take,zip,unzip)
import Data.Stream.Infinite (Stream ((:>)),iterate,tail,repeat,take,zip,unzip,unfold)
import Data.Foldable
import Data.Traversable (Traversable(..), sequence)
import Control.Applicative
import Control.Comonad
import Control.Comonad.Trans.Class (lower)
import Control.Comonad.Trans.Env (EnvT(..),ask)
import System.Random (StdGen,mkStdGen)
import Control.Monad.Random
import Control.Monad.Random.Class

-- Inspired by http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
-- and http://demonstrations.wolfram.com/SimpleProbabilisticCellularAutomata/

data U x = U (Stream x) x (Stream x) deriving (Functor,Foldable)

-- The default instance of Traversable generated by DeriveTraversable wasn't working
-- well with MonadRandom, because U contains infinite structures. If I used the default
-- "sequence" function to transform a U (Rand StdGen Bool) into a Rand StdGen (U Bool),   
-- the subsequent call to evalRand would hang.  
-- I had to resort to this trick of traversing the left and right streams jointly as 
-- a "zip stream" and unzipping them afterwards.
-- Is there a standard, generally accepted way of dealing with these situations?
instance Traversable U where
    traverse f (U lstream focus rstream) = 
        let pairs = liftA unzip . sequenceA . fmap (traversepair f) $ zip lstream rstream 
            traversepair f (a,b) = (,) <$> f a <*> f b
            rebuild c (u,v) = U u c v
        in rebuild <$> f focus <*> pairs

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

-- Advances the cellular automata by one iteration, and returns a result
-- wrapped in the Rand monad, which can be later evaluated with evalRand.
-- Note the use of "sequence" to aggregate the random effects of each 
-- individual cell.
evolve :: EnvT Probs U Bool -> Rand StdGen (EnvT Probs U Bool)
evolve ca = sequence $ extend localRule ca

-- Returns an infinite stream with all the succesive states of the cellular automata.
history :: StdGen -> EnvT Probs U Bool -> Stream (EnvT Probs U Bool)
history seed initialca = 
    -- We need to split the generator because we are lazily evaluating
    -- an infinite random structure. The updated generator never "comes out"! 
    -- If we changed runRand for evalRand and tried to reuse the 
    -- updated generator for the next iteration, the call would hang. 
    let unfoldf (ca,seed) = 
            let (seed',seed'') = runRand getSplit seed 
                nextca = evalRand (evolve ca) seed'
            in  (nextca,(nextca,seed''))
    in unfold unfoldf (initialca,seed)

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
    sequence . fmap (putStrLn . showca margin . lower) 
             . take iterations 
             . history (mkStdGen seed) 
             $ initialca
    return ()

