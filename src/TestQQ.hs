{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module TestQQ (testQQ) where

import Task ( task, combine )

testQQ :: IO ()
testQQ = do
    let x = [task|task = return "23"
theModulus {
module Snippet (theModulus) where

import Test.QuickCheck.Gen

theModulus = do 
    x <- generate $ choose (5, 20) `suchThat` is_prime
    return (show x)

is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
           | otherwise = True
}
-------
module Main where

import Test.QuickCheck

{- Recall the stuff from lecture slide 123. Also, remember Task #{task} 
from last week. Here is a function:

-}

original :: [Integer] -> Integer

original [] = 7

original (x:xs) | (x `mod` #{theModulus}) == 0 = 13 + original xs

| otherwise = x * original xs

-- Reimplement it with foldr:

alternative :: [Integer] -> Integer

alternative = foldr undefined undefined

-- The obvious test suite (but we'll do more elaborate stuff in 
Autotool's feedback as well):

main :: IO ()

main = quickCheck $ list -> original list == alternative list

-----------------

# From here on comes hidden stuff that students do not get to see:

configGhcErrors:

- deprecation

- empty-enumerations

- identities

- name-shadowing

- overflowed-literals

- overlapping-patterns

- tabs

configHlintErrors:

- Avoid reverse

- Collapse lambdas

- Eta reduce

- Evaluate

- Length always non-negative

- Move brackets to avoid $

- Redundant $

- Redundant /=

# ... and many more configuration options, as well as further sections 
as follows, for different purposes:

--------------

{- the hidden testing module -}

module Test (test) where

import qualified Main

import TestHelper (qcWithTimeoutAndRuns)

import Test.HUnit ((~:), Test)

test :: [ Test ]

test = ...

--------------

{- possibly further modules ... -}|]
    res <- combine x x
    print res
