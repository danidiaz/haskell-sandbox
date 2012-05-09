{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Environment
import Data.List
import Data.Maybe
import Data.Typeable
import qualified Data.Map as M

data Component a = Component
    {
        nodeId::Int,
        whatever::a,
        children::[AnyComponent]
    } deriving Typeable

data AnyComponent = forall a . ( Typeable a ) => AnyComponent (Component a) 
                                                       

casty::AnyComponent -> Maybe (Component Int)
casty (AnyComponent c) = cast c

main :: IO ()
main = do 
        args <- getArgs
        -- the following cast should fail
        case casty (AnyComponent (Component 9 "33" [])) of
            Just (Component {})  -> putStrLn "something"
            Nothing -> putStrLn "nothing"
        -- the following cast should work
        case casty (AnyComponent (Component 34 (7::Int) [])) of
            Just (Component {})  -> putStrLn "something"
            Nothing -> putStrLn "nothing"
