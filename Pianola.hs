{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Prelude hiding (catch,(.))
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Category
import Control.Monad
import Control.Applicative
import Control.Error
import qualified Data.Text as T

newtype Nullipotent m a = Nullipotent { runNullipotent:: m a } 
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance (Monad m) => Monad (Nullipotent m) where
    (Nullipotent m) >>= k = Nullipotent $ m >>= (runNullipotent . k)
    return = Nullipotent . return

data Sealed m = Sealed { 
        unseal:: m (),
        tags:: [T.Text]
    }


