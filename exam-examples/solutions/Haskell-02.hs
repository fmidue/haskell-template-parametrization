module Main where
import Test.QuickCheck

-- Every list comprehension can be expressed via map, filter, and
-- concat.
--
-- Here is a concrete list comprehension:

original :: [ (Integer, Integer) ]
original = [ (u,v) | u <- [#{start}..100], v <- [1..u], u `mod` v > 0 ]

-- Your task is to implement the same without using list
-- comprehensions (though range expressions #{wordingWatermark} allowed).
--
-- A rough idea is already given here. Complete that definition
-- by replacing the occurrences of 'undefined' appropriately.
--
-- Do not use any additional top-level or local (let, where)
-- definitions.

alternative :: [ (Integer, Integer) ]
alternative = concat (map (\u -> map (\v -> (u,v)) (filter (\v -> u `mod` v > 0) [1..u])) [#{start}..100])

-- By executing 'main' below, you can partially test your solution
-- before uploading it.

main :: IO ()
main = do putStrLn "Checking prefixes:"
          quickCheck
           (
            forAllShrink (growingElements [1 .. length original]) shrink
              (\n -> take n original == take n alternative)
           )
