configGhcErrors:
- deprecation
- empty-enumerations
- identities
# - name-shadowing # shadowing is natural here, introducing top-level functions for 'simpleEnough' etc.
- overflowed-literals
- overlapping-patterns
- tabs
configHlintErrors:
- Avoid reverse
- Collapse lambdas
- Eta reduce
- Evaluate
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant if
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use &&
- Use /=
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use camelCase
- Use drop
- Use elem
- Use even
- Use fst
- Use guards
- Use head
- Use id
- Use if
- Use init
- Use last
- Use left fold instead of right fold
- Use list literal pattern
- Use odd
- Use otherwise
- Use product
- Use right fold instead of left fold
- Use snd
- Use sum
- Use take
- Use ||
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
allowAdding: true
allowModifying: false
allowRemoving: false
configHlintGroups:
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Apply De Morgan law
- Avoid lambda
- Avoid lambda using `infix`
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
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
- Use section
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
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
 - algorithm (https://en.wikipedia.org/wiki/Merge_sort).
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
 - in Task19.
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
import qualified Main
import Test.QuickCheck
import Test.HUnit ((~:), (@?=), Test, Assertion, assertFailure)
import Data.List

import TestHelper (qcWithTimeoutAndRuns)

test :: [ Test ]
test =
  [ " Test with empty list"
    ~: Main.sort [] @?= []
  , " Test with random inputs"
    ~: qcWithTimeoutAndRuns 5000 250 $ \ns -> Main.sort ns == sort (ns :: [Integer])
  ]
