{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Either
import Control.Monad.List
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Free
import Control.Lens
import Control.Lens.Traversal
import Control.Lens.Fold
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import Data.Tree
import Data.Typeable
import Control.Comonad

foo = Node 23 [Node 34 [], Node 44 [Node 7 [Node 33 []]]]

data Ttree a
   = Bin (Ttree a) (Ttree a)
   | Tip a
   deriving (Eq,Ord,Show,Read,Data,Typeable)

data A = A [A] B deriving (Eq,Ord,Show,Read,Data,Typeable)
data B = B [C] deriving (Eq,Ord,Show,Read,Data,Typeable)
data C = C Int [A] deriving (Eq,Ord,Show,Read,Data,Typeable)

aaa = A [A [] (B [])] (B [C 5 [A [] (B [])]])

instance Data a => Plated (Ttree a) where
   plate = uniplate

treeish = Bin (Tip (Bin (Tip 1) (Tip 2))) (Tip (Bin (Tip 8) (Tip 7))) :: Ttree (Ttree Int)

main :: IO ()
main = return ()
