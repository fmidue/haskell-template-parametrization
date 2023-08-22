enableWhitespaceWatermarking = return "True"
wordingWatermark1 = withCurrentSeed (elements ["all the", "the four"])
wordingWatermark2 = withSeed (elements ["now have to use it", "have to use it now"]) (#{seed} + 1)
wordingWatermark3 = withSeed (elements ["below definition", "definition below"]) (#{seed} + 2)
wordingWatermark4 = withSeed (elements ["really", "actually"]) (#{seed} + 3)
----------
# the seed used was: #{seed}
configGhcErrors:
- empty-enumerations
- overflowed-literals
configHlintErrors:
allowAdding: true
allowModifying: false
allowRemoving: false
configHlintGroups:
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
configHlintRules:
configHlintSuggestions:
- Used otherwise as a pattern
- Using all on tuple
- Using and on tuple
- Using any on tuple
- Using concat on tuple
- Using elem on tuple
- Using foldr on tuple
- Using length on tuple
- Using maximum on tuple
- Using minimum on tuple
- Using null on tuple
- Using or on tuple
- Using product on tuple
- Using sum on tuple
configLanguageExtensions:
- NoTemplateHaskell
- TupleSections
# configLanguageExtensions - this sets LanguageExtensions for hlint as well
# configHlintSuggestions   - hlint hints to provide
# configHlintErrors        - hlint hints to enforce
# configGhcWarnings        - GHC warnings to provide as hints
# configGhcErrors          - GHC warnings to enforce
----------
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

countSatisfying :: (Int -> Bool) -> (Int, Int) -> Int
countSatisfying p =
  divideAndConquer
  undefined
    -- the predicate to test whether a range is simple enough,
    -- should be of type (Int, Int) -> Bool here
  undefined
    -- the function to compute the output directly for simple cases,
    -- should be of type (Int, Int) -> Int here
  undefined
    -- the function to split a non-simple range into two smaller ones
  undefined
    -- the function to combine the outputs of two subcomputations

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
----------
module Test (test) where
import TestHelper (qcWithTimeout)
import qualified Main
import Test.HUnit ((~:), (@?=), Test)
import Test.QuickCheck

test :: [[ Test ]]
test = [
  [ "Counting in empty interval"
    ~: Main.countSatisfying (const True) (1,-1) @?= 0
  , "Positive test cases" -- tests with predicates of form x `mod` k == 0
    ~: qcWithTimeout 10000 $ forAll positiveInputs $ \(Func EQ k p,l,u) -> Main.countSatisfying p (l,u) >= ((u - l + 1) `div` k)
  , "Negative test cases"
    ~: qcWithTimeout 10000 $ forAll negativeInputs $ \(Func _ _ p,l,u) -> Main.countSatisfying p (l,u) == 0
  ]]

instance Show Func where
  show (Func LT k _) = "\\x -> x < " ++ show k
  show (Func EQ k _) = "\\x -> x `mod` " ++ show k ++ " == 0"
  show (Func GT k _) = "\\x -> x > " ++ show k

data Func = Func Ordering Int (Int -> Bool)

positiveInputs :: Gen (Func,Int,Int)
positiveInputs = do
  (l,u) <- nonEmptyInterval
  k <- choose (l,u) `suchThat` (/= 0)
  let k' = abs k
  let p = Func EQ k' (\x -> x `mod` k' == 0)
  return (p,l,u)

negativeInputs :: Gen (Func,Int,Int)
negativeInputs = do
  (l,u) <- nonEmptyInterval
  b <- arbitrary
  let p = if b then Func LT l (< l) else Func GT u (> u)
  return (p,l,u)

nonEmptyInterval :: Gen (Int,Int)
nonEmptyInterval = do
  m <- choose (0,199)
  n <- choose (0,200-m)
  return $ zip [-100..100] (drop m [-100..100]) !! n
