{-# LANGUAGE Rank2Types #-}

import Prelude hiding ((.))
import Control.Category
import Control.Monad    
import Control.Arrow
import Control.Applicative

klei = WrapArrow . Kleisli . const . Just $ (4::Int)

kleif = fmap succ klei

ukleif = (runKleisli . unwrapArrow $ kleif) (0::Int)

data Foo a b = Foo { unFoo::forall m. Monad m => a -> m b }

instance Functor (Foo a) where
    fmap f (Foo z) = Foo (((fmap.liftM)) f z)  
