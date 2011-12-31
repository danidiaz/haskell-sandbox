{-# LANGUAGE Rank2Types, ImplicitParams #-}

import Control.Monad.Reader 
import Control.Monad.Trans 

import System.Environment

-- Copied from here:
-- http://pchiusano.blogspot.com/2011/12/programmatic-translation-to-iteratees.html
-- https://gist.github.com/1525429

-- Skip nodes let us do filtering without lookahead
-- uncons is then a simple loop that removes any Skip nodes
data Step a s = Empty | Yield a s | Skip s

newtype ListT f a = ListT { runListT :: f (Step a (ListT f a)) }

instance Monad f => Monad (ListT f) where
  return a = ListT (return (Yield a empty))
  
  (ListT s) >>= f = ListT $
    s >>= \step -> case step of  
      Yield h t -> return $ (Skip $ f h `mplus` (t >>= f))
      Skip t -> return $ Skip (t >>= f)
      Empty -> return Empty

instance (Monad f) => MonadPlus (ListT f) where
  mzero = ListT (return Empty)

  (ListT s) `mplus` b = ListT $ 
    s >>= \step -> case step of  
      Yield h t -> return $ Yield h (t `mplus` b)
      Skip t -> return $ Skip (t `mplus` b)
      Empty -> return $ Skip b

instance MonadTrans ListT where
  lift fa = ListT $ liftM (\a -> Yield a empty) fa

empty :: Monad f => ListT f a
empty = ListT (return Empty)

cons :: Monad f => a -> ListT f a -> ListT f a
cons h t = ListT . return $ Yield h t

data Input a = Done | Await | Element a 

isDone :: Input a -> Bool
isDone Done = True 
isDone _ = False

prompts :: ListT (Reader (Input a)) a 
prompts = ListT . reader $ \input -> case input of
  Done -> Empty
  Await -> Skip prompts
  Element a -> Yield a prompts

promptsT :: Monad f => ListT (ReaderT (Input a) f) a 
promptsT = ListT . ReaderT $ \input -> case input of
  Done -> return Empty
  Await -> return $ Skip promptsT
  Element a -> return $ Yield a promptsT

type IsEOF = Bool

data Moore a b = Feed b (Input a -> Moore a b) | Stop b IsEOF
data MooreT f a b = FeedT b (Input a -> f (MooreT f a b)) | StopT b IsEOF

invert :: (forall f . Monad f => ListT f a -> ListT f b) 
         -> Moore a (Maybe b)
invert f = go Nothing (f prompts) where
  go cur res = Feed cur $ \a -> case runReader (runListT res) a of
    Empty -> Stop cur (isDone a)
    Yield h t -> go (Just h) t
    Skip s -> go cur s

invertT :: Monad f => (forall t . (MonadTrans t, Monad (t f)) => ListT (t f) a -> ListT (t f) b) -> MooreT f a (Maybe b) 
invertT f = go Nothing (f promptsT) where
  go cur res = FeedT cur $ \a -> do 
    step <- runReaderT (runListT res) a
    return $ case step of 
      Empty -> StopT cur (isDone a)
      Yield h t -> go (Just h) t
      Skip s -> go cur s

main :: IO ()
main = 
    do
       args <- getArgs 
       return () 
