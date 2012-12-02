{-# LANGUAGE Rank2Types #-}

import Prelude hiding ((.))
import Control.Category
import Control.Arrow
import Control.Applicative

klei = WrapArrow . Kleisli . const . Just $ (4::Int)

kleif = fmap succ klei

ukleif = (runKleisli . unwrapArrow $ kleif) (0::Int)

data Foo = Foo (forall b. Monad b => Int -> b Int)
