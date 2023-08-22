enableWhitespaceWatermarking = return "True"
moduleName = return "Task25"
otherTask = return "Task19"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
# - name-shadowing # shadowing is natural here, introducing top-level functions for 'simpleEnough' etc.
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
- Use odd
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- unused-local-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Use ++
- Use 1
- Use all
- Use and
- Use any
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use find
- Use floor
- Use foldl
- Use foldr
- Use fromMaybe
- Use infix
# - Use isJust
# - Use isNothing
- Use lefts
- Use list comprehension
- Use map
- Use map once
- Use mapMaybe
- Use maximum
# - Use maybe
- Use minimum
- Use negate
- Use newtype instead of data
- Use notElem
# - Use null
- Use or
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Prelude hiding (filter, any, all)
import Test.QuickCheck

-- We again want to use the divideAndConquer function from #{otherTask}.

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
 - in #{otherTask}.
 -
 - Hint: Make sure you do not forget to handle empty intervals, i.e.,
 -       intervals where the lower bound is greater than the upper
 -       bound.
 -}

findSatisfying :: (Int -> Bool) -> Int -> Int -> Bool
findSatisfying p = curry $ -- making initial input (from, to)
                           -- available to the recursion, at type
                           -- (Int, Int) here
  divideAndConquer
  undefined
    -- the predicate to test whether an input is simple enough,
    -- should be of type (Int, Int) -> Bool here
  undefined
    -- the function to compute the output directly for simple cases,
    -- should be of type (Int, Int) -> Bool here
  undefined
    -- the function to split a non-simple input into two smaller ones,
    -- should be of type (Int, Int) -> ((Int, Int), (Int, Int)) here
  undefined
    -- the function to combine the outputs of two subcomputations,
    -- should be of type Bool -> Bool -> Bool here

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
----------
module Test (test) where
import TestHelper (qcWithTimeout)
import qualified #{moduleName}
import Test.HUnit ((~:), (@?=), Test)
import Test.QuickCheck

test :: [[ Test ]]
test = [
  [ "Searching in empty interval"
    ~: #{moduleName}.findSatisfying (const True) 1 (-1) @?= False
  , "Positive test cases"
    ~: qcWithTimeout 5000 $ forAll positiveInputs $ \(Func _ _ p,l,u) -> #{moduleName}.findSatisfying p l u
  , "Negative test cases"
    ~: qcWithTimeout 5000 $ forAll negativeInputs $ \(Func _ _ p,l,u) -> not $ #{moduleName}.findSatisfying p l u
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
