module Main where
import Test.QuickCheck

-- Here is a recursive function on lists of integers:

original :: [Integer] -> Integer
original [] = 1
original (x:xs) | #{predicate}    = x - #{factor} * original xs
                | otherwise = original xs

-- Your task is to express the above function as an application of
-- 'foldr'. Just replace the two occurrences of 'undefined' in the
-- following definition, #{wordingWatermark} that 'original' and 'alternative'
-- compute the same mathematical function.

alternative :: [Integer] -> Integer
alternative = foldr (\x y -> if #{predicate} then x - #{factor} * y else y) 1

-- The obvious test suite:

main :: IO ()
main = quickCheck $ \l -> original l == alternative l
