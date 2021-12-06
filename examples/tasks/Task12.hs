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
import Prelude hiding (($), (!!), take, drop, elem, notElem, any, all, and, or, map, fmap, filter, foldr, foldl)
import Test.QuickCheck

{-
Write a function 'notElem' for the given type signature which returns
whether an integer is not an element of a given list.

You should not use pattern-matching on binary list constructors or the
empty list, even if you happen to already know this concept.

Use exactly one equation (maybe with guards) to define 'notElem'. If
you feel a need to use 'where', use 'let' instead.

By executing 'main' below, you can test your solution before uploading
it.
-}

notElem :: Integer -> [Integer] -> Bool
notElem v xs = undefined

main :: IO ()
main = do putStrLn "Empty list does not contain anything:"
          quickCheck (`notElem` [])
          putStrLn "Singleton list [x] contains x:"
          quickCheck (\x -> not (notElem x [x]))
          putStrLn "Singleton list [y] does not contain x if x /= y:"
          quickCheck (\x y -> x /= y ==> notElem x [y])
          putStrLn "A combined list lacks v exactly if it is not contained in either of the separate lists:"
          quickCheck (\v xs ys -> notElem v (xs ++ ys) == (notElem v xs && notElem v ys))
---------
module Test (test) where
import TestHelper (qcWithTimeout)
import TestHarness
import Test.HUnit (Test, (@=?), (@?), (~:))
import qualified Main

test :: [[ Test ]]
test =
  [[
    " Testing 'notElem 4 [1..10]'"
    ~: False @=? Main.notElem 4 [1..10],
    " Testing 'notElem 7 [9,8,7,6,5]'"
    ~: False @=? Main.notElem 7 [9,8,7,6,5],
    " Testing 'notElem 0 []'"
    ~: True @=? Main.notElem 0 [],
    " Testing 'notElem 4 [9,8,7,6,5]'"
    ~: True @=? Main.notElem 4 [9,8,7,6,5],
    "Testing with random values:"
    ~: qcWithTimeout 5000 $ \v xs -> Main.notElem v xs == Prelude.notElem v xs
  ]]
