enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
wikipedia = return "https://en.wikipedia.org/wiki/Merge_sort"
otherTask = return "Task19"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
# - name-shadowing # shadowing is natural here, introducing top-level functions for 'simpleEnough' etc.

#{commonConfigHlintErrors}
- Eta reduce
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
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
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
  undefined
  undefined
  undefined
  undefined

-- Here is a useful test suite:
main :: IO ()
main = do
  quickCheck $ \(Positive n) -> sort [1..n] == [1..n]
  quickCheck $ \xs -> sort (reverse xs) == sort xs
  quickCheck $ \xs ys -> sort (xs ++ ys) == sort (ys ++ xs)
  quickCheck $ \xs -> length (sort xs) == length xs
  quickCheck $ \xs -> sort (xs ++ xs) == concat [ [x,x] | x <- sort xs ]
----------
module Test (test) where
import qualified #{moduleName}
import Test.QuickCheck
import Test.HUnit ((~:), (@?=), Test, Assertion, assertFailure)
import Data.List

import TestHelper (qcWithTimeoutAndRuns)

test :: [ Test ]
test =
  [ " Test with empty list"
    ~: #{moduleName}.sort [] @?= []
  , " Test with random inputs"
    ~: qcWithTimeoutAndRuns 500000 250 $ \ns -> #{moduleName}.sort ns == sort (ns :: [Integer])
  ]
