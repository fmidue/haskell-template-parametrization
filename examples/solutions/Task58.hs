module Main where
import Prelude hiding ((!!), head, tail, last, init, take, drop, splitAt, dropWhile, zip, zipWith)
import Test.QuickCheck

{- Implement, via pattern-matching, a function which combines repeated
 - values occurring next to each other, into single entries. For
 - example:
 -
 -   compress [1,3,3,3,2,4,4,2,4] = [1,3,2,4,2,4]
 -}

compress :: [Integer] -> [Integer]
compress []        = []
compress [x]       = [x]
compress (x:y:zs)  = if x == y then compress (y:zs) else x : compress (y:zs)

main :: IO ()
main = do putStrLn "executing first test set:"
          quickCheck $ \n -> compress [1..n] == [1..n]
          putStrLn "executing second test set:"
          quickCheck $ \xs -> length (compress xs) <= length xs
          putStrLn "executing third test set:"
          quickCheck $ \xs y zs -> compress (xs ++ [y,y] ++ zs) == compress (xs ++ [y] ++ zs)
