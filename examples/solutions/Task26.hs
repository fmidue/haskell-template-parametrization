module Main where
import Test.QuickCheck

{- Here is a recursive function on lists of integers. It is not really
 - important what it does and why, but it obviously does it by
 - structural recursion:
 -}

original :: [Integer] -> Integer
original [] = 7
original (x:xs) | (x `mod` 11) == 0 = 13 + original xs
                | otherwise         = x * original xs

{- The 'foldr' function shown in the lecture was said to exactly
 - capture structural recursion on lists. So it should be possible to
 - express the above function as an application of 'foldr'. That is
 - your task. Replace the two occurrences of 'undefined' in the
 - following definition, such that 'original' and 'alternative'
 - compute the same mathematical function.
 -
 - Do not use any additional top-level or local (let, where)
 - definitions.
 -}

alternative :: [Integer] -> Integer
alternative = foldr (\x y -> if (x `mod` 11) == 0 then 13 + y else x * y) 7

-- The obvious test suite:
main :: IO ()
main = quickCheck $ \list -> original list == alternative list
