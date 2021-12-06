configGhcErrors:
- deprecation
- empty-enumerations
- identities
- missing-signatures
# - name-shadowing # shadowing is natural here, introducing top-level functions for 'simpleEnough' etc.
- overflowed-literals
- overlapping-patterns
- tabs
- unused-matches
- unused-pattern-binds
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
- Use notElem
- Use odd
- Use otherwise
- Use product
- Use right fold instead of left fold
- Use snd
- Use splitAt
- Use sum
- Use tail
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
# - incomplete-patterns # might reveal list patterns, and would advise against (here possibly desirable) partial definition for 'simpleCases'
# - incomplete-uni-patterns # might reveal list patterns
- unused-local-binds
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
- Use map once
- Use mapMaybe
- Use maximum
# - Use maybe
- Use minimum
- Use negate
- Use newtype instead of data
# - Use null
- Use or
- Use repeat
- Use replicate
- Use rights
- Use section
- Use sqrt
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
import Prelude hiding (($), sum, take, drop, tail)
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
  undefined
    -- the predicate to test whether an input is simple enough,
    -- should be of type [Integer] -> Bool here
  undefined
    -- the function to compute the output directly for simple cases,
    -- should be of type [Integer] -> Integer here
  undefined
    -- the function to split a non-simple input into two smaller ones,
    -- should be of type [Integer] -> ([Integer], [Integer]) here
  undefined
    -- the function to combine the outputs of two subcomputations,
    -- should be of type Integer -> Integer -> Integer here

main :: IO ()
main = do putStrLn "If your code does not even satisfy this, there is a problem:"
          quickCheck (sum [] == 0)
----------
module Test (test) where
import qualified Main
import Test.QuickCheck
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), (@?=), Test)

test :: [ Test ]
test =
  [ " Test with empty list"
    ~: Main.sum [] @?= 0,
    " Test with random inputs"
    ~: qcWithTimeout 5000 $ \ns -> not (null ns)
                   ==> Main.sum ns == sum ns
  ]
