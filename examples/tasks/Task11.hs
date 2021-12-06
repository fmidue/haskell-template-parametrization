configGhcErrors:
- deprecation
- empty-enumerations
- identities
# - incomplete-patterns # might reveal list patterns
# - incomplete-uni-patterns # might reveal list patterns
- missing-signatures
- name-shadowing
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
# - Use guards # not necessarily
- Use head
- Use id
- Use if
- Use init
# - Use isJust
# - Use isNothing
- Use last
- Use left fold instead of right fold
- Use list literal pattern
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use otherwise
- Use product
- Use replicate
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
allowAdding: false
allowModifying: false
allowRemoving: false
configHlintGroups:
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
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
- Use lefts
- Use list comprehension
- Use map once
- Use mapMaybe
# - Use maybe
- Use negate
- Use newtype instead of data
- Use notElem
- Use or
- Use repeat
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
import Prelude hiding (($), (!!), tail, take, drop, foldr, foldl, sum)
import Test.QuickCheck

{-
 - Write a function 'sum' which computes the sum of a (finite)
 - list of numbers, so that for example: sum [2,5,3] == 10.
 -
 - You should not use pattern-matching on binary list constructors
 - here, even if you happen to already know this concept. Instead, use
 - a divide-and-conquer approach. That is, we expect to see something
 - like 'splitAt (length list `div` 2) list' in your solution.
 -
 - Use exactly one equation (maybe with guards) to define 'sum'. If
 - you feel a need to use 'where', use 'let' instead.
 -}

sum :: [Integer] -> Integer
sum xs = undefined

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
  [
    " Test with empty list"
    ~: Main.sum [] @?= 0,
    " Test with random inputs"
    ~: qcWithTimeout 5000000 $ \ns -> not (null ns)
                   ==> Main.sum ns == sum ns
  ]
