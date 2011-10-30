module EnumeratorExamples (
        enumeratorExample1,
        enumeratorExample2,
        enumeratorExample3,
        enumeratorExample4
) where

import Control.Monad
import Control.Exception 

import Data.List

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

enumeratorExample1 = 
    let
        enum = E.enumList 2 [1..3::Int] 
        iter = EL.fold (+) 0 
    in  E.run $ enum E.$$ iter                 

-- Iteratees seem to compose with plain old monadic (>>=)
-- See http://www.mew.org/~kazu/proj/enumerator/
enumeratorExample2 = 
    let
        enum = E.enumList 2 [1..3::Int] 
        startIterWith n = EL.fold (+) n 
    in  E.run $ enum E.$$ (EL.head_ >>= startIterWith)                

-- How dows head work on an infinite list?
enumeratorExample3 = 
        E.run $ EL.repeat 1 E.$$ EL.head_

-- Enumerators seem to compose with (>==>)
-- "The moral equivalent of (>=>) for iteratees." 
enumeratorExample4 = 
    let
        enum1 = E.enumList 1 [1..3::Int] 
        enum2 = E.enumList 1 [5..7::Int] 
        iter = EL.fold (+) 0 
    in  E.run $ enum1 E.>==> enum2 E.$$ iter                 

