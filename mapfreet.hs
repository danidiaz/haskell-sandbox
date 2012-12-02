{-# LANGUAGE TemplateHaskell,ScopedTypeVariables,GeneralizedNewtypeDeriving,FlexibleInstances,Rank2Types #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Either
import Control.Monad.List
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Free

mapFreeT::  (Functor f, Functor m) => (forall a. m a -> m' a) -> FreeT f m a -> FreeT f m' a
mapFreeT f (FreeT m) = FreeT (f ((fmap.fmap) (mapFreeT f) m))

main :: IO ()
main = do 
        putStrLn ""
