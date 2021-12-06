module Main where
import Prelude hiding ((!!))
import Test.HUnit
import Test.QuickCheck

{- Define your own Haskell data type for representing non-empty trees
 - of integer numbers, where nodes can have arbitrarily many children.
 -}

data Tree = Node Integer [Tree]

{- Give a value of your data type that corresponds to a tree
 - containing the integers 1, 2, 4, 5, 6, 8, but no others.
 -}

tree :: Tree
tree = Node 5 [Node 1 [Node 6 []], Node 8 [], Node 2 [Node 1 [], Node 4 []]]

{- Write a function that checks for a given number and tree whether
 - the number occurs in the tree.
 -}

contains :: Tree -> Integer -> Bool
contains (Node n xs) m = m==n || contains' xs m

contains' :: [Tree] -> Integer -> Bool
contains' [] _ = False
contains' (x:xs) n = contains x n || contains' xs n

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "a unit test" ~:
            tree `contains` 3 @?= False
          quickCheck $ forAll (elements [1,2,4,5,6,8]) $ \n -> tree `contains` n
