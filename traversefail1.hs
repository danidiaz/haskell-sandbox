{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Prelude hiding (catch,(.),iterate,tail,repeat,sequence,take)
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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let (runny,seedy) = runRand (sequence listystp) (mkStdGen 77)
    -- putStrLn . show $ seedy  -- if we require the seed, the computation hangs
    mapM_ (putStrLn . show) $ take 10 $ runny
    return ()

