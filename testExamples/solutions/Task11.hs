module Main where
import Prelude hiding (($), (!!), tail, take, drop, foldr, foldl, #{name})
import Test.QuickCheck

{-
 - Write a function '#{name}' which computes the #{name} (x#{operator}y) of a (finite)
 - list of numbers, so that for example: #{name} #{plain_exampleList} == #{result}.
 -
 - 
 - You should not use pattern-matching on binary list constructors
 - here, even if you happen to already know this concept. Instead, use
 - a divide-and-conquer approach. That is, we expect to see something
 - like 'splitAt (length list `div` 2) list' in your solution.
 -
 - Use exactly one equation (maybe with guards) to define '#{name}'. If
 - you feel a need to use 'where', use 'let' instead.
 -}

#{name} :: [Integer] -> Integer
#{name} xs
 | xs == [] = #{neutralElement}
 | otherwise  =
  let
    n = length xs
  in
    if n == 1
    then head xs
    else
      let (as,bs) = splitAt (n `div` 2) xs
      in #{name} as #{operator} #{name} bs

main :: IO ()
main = do putStrLn "If your code does not even satisfy this, there is a problem:"
          quickCheck (#{name} [] == #{neutralElement})
