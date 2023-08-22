module Main where
import Prelude hiding (filter)
import Test.QuickCheck

-- Recall how a divide-and-conquer approach was expressed as a
-- higher-order-function in the course.
--
-- From the verbal description:
--
--   First we need to have some predicate that tells us when an input
--   is simple enough to not need further dividing. Given some
--   concrete input, we apply this predicate. If it tells us that the
--   input is simple enough, we apply some function that for the
--   simple cases computes the output directly. If the predicate tells
--   us that the input is not yet simple enough, we split it into two
--   smaller inputs somehow, as determined by another function, in a
--   way such that the two parts are roughly of equal size. Then we
--   recursively perform the computation for these two smaller
--   inputs. Finally we combine the outputs from the two
--   subcomputations, using yet another function.
--
-- ... the following higher-order function was obtained which
-- abstracts over the mentioned predicate and functions for direct
-- computation of simple cases, splitting, and combining:

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

-- You #{wordingWatermark2} to implement a function 'countSatisfying'
-- that counts, for a predicate p, how many integers in a given range
-- satisfy p, i.e., for how many numbers x in that range p x holds.
--
-- Complete the #{wordingWatermark3} by replacing #{wordingWatermark1} occurrences of
-- 'undefined' accordingly.
--
-- Do really follow the divide-and-conquer principle. That is, choose
-- argument functions that #{wordingWatermark4} fit the roles of checking for
-- simplicity, splitting into about equally sized parts, etc., as
-- described above.
--
-- Hint: Make sure you do not forget to handle empty intervals, i.e.,
--       intervals where the lower bound is greater than the upper
--       bound.

countSatisfying' :: (Int -> Bool) -> (Int, Int) -> Int
countSatisfying' p =
  divideAndConquer
  isSimple
  solveSimple
  divide
  (+)
  where
    isSimple (lower,upper) = upper - lower < 2
    solveSimple (lower, upper)
      | lower > upper = 0
      | lower == upper && p lower = 1
      | otherwise = countSingle lower + countSingle upper
    countSingle x
      | p x = 1
      | otherwise = 0
    divide (lower, upper) =
      let middle = (lower + upper) `div` 2
      in ((lower, middle), (middle + 1, upper))

main :: IO ()
main = do putStrLn "Nothing to count in empty ranges"
          quickCheck (\i -> countSatisfying (const True) (i, i - 1) == 0)
          putStrLn "Nothing to count with vacuous predicate"
          quickCheck (\i j -> countSatisfying (const False) (i, j) == 0)
          putStrLn "Some positive test cases"
          quickCheck (\xs ys zs -> count42 (xs ++ [42] ++ ys ++ [42] ++ zs) >= 2)
          putStrLn "Some negative test cases"
          quickCheck (\xs -> (42 `notElem` xs) ==> (count42 xs == 0))
  where
    count42 :: [Int] -> Int
    count42 list = countSatisfying ((42 ==) . (list !!)) (0, length list - 1)


-- alternative solution:

countSatisfying :: (Int -> Bool) -> (Int, Int) -> Int
countSatisfying p =
  divideAndConquer
  (\(lower, upper) -> upper - lower < 1)
  (\(lower, upper) -> if lower == upper && p lower then 1 else 0)
  (\(lower, upper) ->
    let middle = (lower + upper) `div` 2
    in ((lower, middle), (middle + 1, upper)))
  (+)
