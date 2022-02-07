module Main where
import Test.QuickCheck

expand :: [Integer] -> [Integer]
expand = concatMap (\z -> [n | n <- [z-5..z]])

original :: [Integer] -> Integer
original [] = 0
original (x:xs) | x < 20    = 5 * x - 3 + original xs
                | otherwise = original xs

alternative :: [Integer] -> Integer
alternative = foldr (\x y -> if x < 20 then 5 * x - 3 + y else y) 0

-- The obvious test suite:
main :: IO ()
main = quickCheck $ \list -> original (expand list) == alternative (expand list)
