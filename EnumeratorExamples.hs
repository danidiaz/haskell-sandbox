module EnumeratorExamples (
        enumeratorExample1,
        enumeratorExample2,
        enumeratorExample3,
        enumeratorExample4,
        enumeratorExample5,
        enumeratorExample6,
        enumeratorExample7
) where

import Control.Monad
import Control.Exception 

import Data.List

import Data.Enumerator
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

enumeratorExample1 = 
    let
        enum = E.enumList 2 [1..3::Int] 
        iter = EL.fold (+) 0 
    in  E.run $ enum $$ iter                 

-- Iteratees seem to compose with plain old monadic (>>=)
-- See http://www.mew.org/~kazu/proj/enumerator/
enumeratorExample2 = 
    let
        enum = E.enumList 2 [1..3::Int] 
        startIterWith n = EL.fold (+) n 
    in  E.run $ enum $$ (EL.head_ >>= startIterWith)                

-- How dows head work on an infinite list?
enumeratorExample3 = 
        E.run $ EL.repeat 1 $$ EL.head_

-- Enumerators seem to compose with (>==>)
-- "The moral equivalent of (>=>) for iteratees." 
enumeratorExample4 = 
    let
        enum1 = E.enumList 1 [1..3::Int] 
        enum2 = E.enumList 1 [5..7::Int] 
        iter = EL.fold (+) 0 
    in  E.run $ enum1 >==> enum2 $$ iter                 

-- Using the "unique" Enumeratee,
-- composing it with an source Enumerator.
-- Remember that joinE is equivalent to ($=)
enumeratorExample5 = 
    let
        enum = E.enumList 1 [1,2,1,2,3,4::Int] 
        iter = EL.fold (+) 0 
    in  E.run $ enum $= EL.unique $$ iter                 

-- Enumeratees can also be composed with Iteratees
-- using the (=$) operator.
enumeratorExample6 = 
    let
        enum = E.enumList 1 [1,2,1,2,3,4::Int] 
        iter = EL.fold (+) 0 
    in  E.run $ enum $$ EL.unique =$ iter                 

-- Feeding a fold with two different enumerators.
-- This could be done in several different ways.
enumeratorExample7 = 
    let
        strEnum = E.enumList 1 ["1","2","3"] 
        intEnum1 = strEnum $= EL.map read 
        intEnum2 = E.enumList 1 [4,5,6::Int] 
        iter = EL.fold (+) 0
    in  E.run $ intEnum2 $$ (intEnum1 $$ iter)

