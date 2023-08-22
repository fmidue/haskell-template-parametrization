module #{moduleName} where
import Prelude hiding (($), (!!), tail, init, take, drop, foldr, foldl, sum)
import Test.QuickCheck

{-
 - Write a function 'sum' which computes the sum of a (finite)
 - list of numbers, so that for example: sum [2,5,3] == 10.
 -
 - You should not use pattern-matching on binary list constructors
 - here, even if you happen to already know this concept. Instead, use
 - a divide-and-conquer approach. That is, we expect to see something
 - like 'splitAt (length list `div` 2) list' in your solution.
 -
 - Use exactly one equation (maybe with guards) to define 'sum'. If
 - you feel a need to use 'where', use 'let' instead.
 -}

sum :: [Integer] -> Integer
sum xs
 | xs == [] = 0
 | otherwise  =
  let
    n = length xs
  in
    if n == 1
    then head xs
    else
      let (as,bs) = splitAt (n `div` 2) xs
      in sum as + sum bs

main :: IO ()
main = do putStrLn "If your code does not even satisfy this, there is a problem:"
          quickCheck (sum [] == 0)
