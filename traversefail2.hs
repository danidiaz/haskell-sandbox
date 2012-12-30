{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Prelude hiding (catch,(.),iterate,tail,repeat,sequence,take,zip)
import Data.Monoid
import Data.Foldable hiding (mapM_)
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

listy :: Stream Float   
listy = 7.0 :> repeat 1.0

listystp :: Stream (Rand StdGen Float)
listystp = fmap (const $ getRandomR (0,1)) listy

data PoS a = PoS (Stream a) (Stream a) deriving (Functor, Foldable, Traversable)

posrnd :: PoS (Rand StdGen Float)
posrnd = PoS (repeat $ getRandomR (0.0,1.0)) (repeat $ getRandomR (0.0,1.0))

rndpos :: Rand StdGen (PoS Float)
rndpos = sequence posrnd

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let (PoS stream1 stream2) = evalRand rndpos (mkStdGen 77)
    -- putStrLn . show $ seedy  -- if we require the seed, the computation hangs
    mapM_ (putStrLn . show) $ take 10 $ zip stream1 stream2
    return ()

