module #{moduleName} where
import Prelude hiding (($), sum, take, drop, tail, init)
import Test.QuickCheck

{- You should know what it means, intuitively, to compute something by
 - a divide-and-conquer approach. One verbal, and somewhat verbose,
 - way to describe divide-and-conquer computation is as follows:
 -
 - First we need to have some predicate that tells us when an input is
 - simple enough to not need further dividing. Given some concrete
 - input, we apply this predicate. If it tells us that the input is
 - simple enough, we apply some function that for the simple cases
 - computes the output directly. If the predicate tells us that the
 - input is not yet simple enough, we split it into two smaller inputs
 - somehow, as determined by another function, in a way such that the
 - two parts are roughly of equal size. Then we recursively perform
 - the computation for these two smaller inputs. Finally we combine
 - the outputs from the two subcomputations, using yet another
 - function.
 -
 - The same ideas can be expressed as a higher-order function which
 - abstracts over the mentioned predicate and functions for direct
 - computation of simple cases, splitting, and combining:
 -}

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

{- Write (again) a function 'sum' which computes the sum of a
 - (finite) list of numbers, so that for example: sum [2,5,3] == 10.
 -
 - Do this by a single call to the above higher-order function. That
 - is, think of what the predicate and functions mentioned abstractly
 - above should do specifically for the problem of sum-of-list
 - computation, and then simply replace the four occurrences of
 - 'undefined' below accordingly.
 -
 - Do really follow the divide-and-conquer principle. That is, choose
 - arguments that actually fit the roles of checking for simplicity,
 - splitting into about equally sized parts, etc., as described
 - further above.
 -}

sum :: [Integer] -> Integer
sum =
  divideAndConquer
  (\list -> length list <= 1)
  (\list -> if null list then 0 else head list)
  (\list -> splitAt (length list `div` 2) list)
  (+)

-- alternative:
sum' :: [Integer] -> Integer
sum' = divideAndConquer simpleEnough simpleCases splitFunction combineFunction

simpleEnough :: [Integer] -> Bool
simpleEnough list = length list <= 1

simpleCases :: [Integer] -> Integer
simpleCases [] = 0
simpleCases [n] = n

splitFunction :: [Integer] -> ([Integer], [Integer])
splitFunction list = splitAt (length list `div` 2) list

combineFunction :: Integer -> Integer -> Integer
combineFunction = (+)

main :: IO ()
main = do putStrLn "If your code does not even satisfy this, there is a problem:"
          quickCheck (sum [] == 0)
