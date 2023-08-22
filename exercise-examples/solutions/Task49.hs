module #{moduleName} where
import Prelude hiding ((!!))
import Test.QuickCheck

-- Recall the following function:

divideAndConquer ::
  (a -> Bool) ->
  (a -> b) ->
  (a -> (a,a)) ->
  (b -> b -> b) ->
  a -> b
divideAndConquer simpleEnough simpleCases splitFunction combineFunction =
  recursively
  where
    recursively input =
      if simpleEnough input then simpleCases input
      else
        let
          (left,right) = splitFunction input
        in
          combineFunction (recursively left) (recursively right)

{- We now want to implement Mergesort, as a typical divide-and-conquer
 - algorithm (#{wikipedia}).
 -
 - Do this by a single call to the above higher-order function. That
 - is, think of what the predicate and functions mentioned abstractly
 - in the explanation of this function in an earlier task should do
 - specifically for the Mergesort algorithm, and then simply replace
 - the four occurrences of 'undefined' below accordingly.
 -
 - Do really follow the divide-and-conquer principle. That is, choose
 - arguments that actually fit the roles of checking for simplicity,
 - splitting into about equally sized parts, etc., as described
 - in #{otherTask}.
 -}

sort :: [Integer] -> [Integer]
sort =
  divideAndConquer
  (\list -> length list <= 1)
  id
  (\list -> splitAt (length list `div` 2) list)
  merge

merge :: [Integer] -> [Integer] -> [Integer]
merge []     ys               = ys
merge xs     []               = xs
merge (x:xs) ys | x < head ys = x : merge xs ys
merge xs     (y:ys)           = y : merge xs ys

-- Here is a useful test suite:
main :: IO ()
main = do
  quickCheck $ \(Positive n) -> sort [1..n] == [1..n]
  quickCheck $ \xs -> sort (reverse xs) == sort xs
  quickCheck $ \xs ys -> sort (xs ++ ys) == sort (ys ++ xs)
  quickCheck $ \xs -> length (sort xs) == length xs
  quickCheck $ \xs -> sort (xs ++ xs) == concat [ [x,x] | x <- sort xs ]
