{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment
import Data.List
import Data.Maybe
import Data.Typeable
import qualified Data.Map as M

data Component = Component
    {
        nodeId::Int,
        children::[AnyComponent]
    } deriving Typeable

class HasComponent a where
    base :: a -> Component
    downcast :: Typeable b => a -> Maybe b

instance HasComponent Component where
    base = id
    downcast = cast

data AnyComponent = forall a . ( Typeable a, HasComponent a ) => AnyComponent a 
                                                       
instance HasComponent AnyComponent where
    base (AnyComponent a) = base a
    downcast (AnyComponent a) = cast a

data Button = Button 
    {
       buttonComponent:: Component,
       foo::Int 
    } deriving Typeable

instance HasComponent Button where
    base = buttonComponent
    downcast = cast 

main :: IO ()
main = do 
        args <- getArgs
        --the following cast should fail
        case downcast (AnyComponent (Button (Component 9 []) 55) ) of
            Just "foo"  -> putStrLn "something"
            Nothing -> putStrLn "nothing"
        --the following cast should work
        case downcast (AnyComponent (Button (Component 34 []) 33)) of
            Just (b::Button)  -> putStrLn "something"
            Nothing -> putStrLn "nothing"
