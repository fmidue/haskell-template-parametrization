module Main where
import Prelude hiding (filter, any, all)
import Test.QuickCheck

-- We again want to use the divideAndConquer function from last week.

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

{- This time we want to implement a function 'findSatisfying' that
 - checks, for a predicate p, if in a given range of integers there is
 - a number x such that p x holds.
 -
 - Complete the definition by replacing the four occurrences of
 - 'undefined' below accordingly.
 -
 - Do really follow the divide-and-conquer principle. That is, choose
 - arguments that actually fit the roles of checking for simplicity,
 - splitting into about equally sized parts, etc., as described
 - in Task19.
 -
 - Hint: Make sure you do not forget to handle empty intervals, i.e.,
 -       intervals where the lower bound is greater than the upper
 -       bound.
 -}

findSatisfying :: (Int -> Bool) -> Int -> Int -> Bool
findSatisfying p = curry $
  divideAndConquer
  (\(lower, upper) -> upper - lower < 2)
  (\(lower, upper) -> lower <= upper && (p lower || p upper))
  (\(lower, upper) ->
    let middle = (lower + upper) `div` 2
    in ((lower, middle), (middle + 1, upper)))
  (||)

main :: IO ()
main = do putStrLn "Nothing to find in empty ranges"
          quickCheck (\i -> not (findSatisfying (const True) i (i - 1)))
          putStrLn "Nothing to find with vacuous predicate"
          quickCheck (\i j -> not (findSatisfying (const False) i j))
          putStrLn "Some positive test cases"
          quickCheck (\xs ys -> has13 (xs ++ [13] ++ ys))
          putStrLn "Some negative test cases"
          quickCheck (\xs -> 13 `notElem` xs ==> not (has13 xs))
  where
    has13 :: [Integer] -> Bool
    has13 list = findSatisfying ((13 ==) . (list !!)) 0 (length list - 1)


-- alternative solution:

findSatisfying' :: (Int -> Bool) -> Int -> Int -> Bool
findSatisfying' p = curry $
  divideAndConquer
  (\(lower, upper) -> upper - lower < 1)
  (\(lower, upper) -> lower == upper && p lower)
  (\(lower, upper) ->
    let middle = (lower + upper) `div` 2
    in ((lower, middle), (middle + 1, upper)))
  (||)
