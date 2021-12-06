{-# LANGUAGE TupleSections #-}
module Main where
import Prelude hiding (($))
import Test.QuickCheck

{- A lecture slide claims that every list comprehension can be
 - expressed via map, filter, and concat instead. Let us try to
 - exemplify this.
 -
 - Here is a concrete list comprehension:
 -}

original :: [ (Integer, Integer) ]
original = [ (x,y) | x <- [-50..50], y <- [x..50], abs (x + y) > 35 ]

{- Your task is to implement the same without using list
 - comprehensions (though range expressions are still allowed).
 -
 - A rough idea is already given here. Complete that definition
 - by replacing the occurrences of 'undefined' appropriately.
 -
 - Do not use any additional top-level or local (let, where)
 - definitions.
 -}

alternative :: [ (Integer, Integer) ]
alternative = concat (map (\x -> map (x,) (filter (\y -> abs (x + y) > 35) [x..50])) [-50..50])

{- By executing 'main' below, you can partially test your solution
 - before uploading it.
 -}

main :: IO ()
main = do putStrLn "Checking prefixes:"
          quickCheck
           (
            forAllShrink (growingElements [1 .. length original]) shrink
              (\n -> take n original == take n alternative)
           )
