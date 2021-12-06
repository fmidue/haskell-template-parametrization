configGhcErrors:
- deprecation
- empty-enumerations
- identities
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
- unused-local-binds
- unused-matches
- unused-pattern-binds
configHlintErrors:
- Apply De Morgan law
- Avoid lambda
- Avoid reverse
- Collapse lambdas
- Eta reduce
- Evaluate
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
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
- Use ++
- Use /=
- Use 1
- "Use :"
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use all
- Use and
- Use any
- Use camelCase
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use drop
- Use elem
- Use even
- Use find
- Use floor
- Use foldr
- Use fromMaybe
- Use fst
- Use guards
- Use head
- Use id
- Use if
- Use infix
- Use init
- Use last
- Use left fold instead of right fold
- Use lefts
- Use list comprehension
- Use list literal pattern
- Use map
- Use map once
- Use mapMaybe
- Use maximum
# - Use maybe
- Use minimum
- Use negate
# - Use newtype instead of data
- Use notElem
- Use odd
- Use or
- Use otherwise
- Use product
- Use repeat
- Use replicate
- Use right fold instead of left fold
- Use rights
- Use snd
- Use splitAt
- Use sqrt
- Use sum
- Use take
- Use tuple-section
# - Use uncurry
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
allowModifying: true
allowRemoving: false
configHlintGroups:
- monomorphic
- teaching
# QuickCheck/HUnit testing happens here
configGhcWarnings: []
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Avoid lambda using `infix`
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
- Use foldl
- Use section
- Use tail
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
import Test.HUnit
import Test.QuickCheck

{- Define your own Haskell data type for representing non-empty trees
 - of integer numbers, where nodes can have arbitrarily many children.
 -}

data Tree = DefineThisHere

{- Give a value of your data type that corresponds to a tree
 - containing the integers 1, 2, 4, 5, 6, 8, but no others.
 -}

tree :: Tree
tree = undefined

{- Write a function that checks for a given number and tree whether
 - the number occurs in the tree.
 -}

contains :: Tree -> Integer -> Bool
contains = undefined

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "a unit test" ~:
            tree `contains` 3 @?= False
          quickCheck $ forAll (elements [1,2,4,5,6,8]) $ \n -> tree `contains` n
-------------------------------------
module Test (test) where
import Prelude
import qualified Main
import Test.QuickCheck
import Test.HUnit ((~:), (@?=),Test)

test :: [[ Test ]]
test = [
       [ " contains tree 3" ~: Main.contains Main.tree 3 @?= False
       , " contains tree 8" ~: Main.contains Main.tree 8 @?= True
       , " contains tree 5" ~: Main.contains Main.tree 5 @?= True
       , " contains tree 1" ~: Main.contains Main.tree 1 @?= True
       , " contains tree 6" ~: Main.contains Main.tree 6 @?= True
       , " contains tree 2" ~: Main.contains Main.tree 2 @?= True
       , " contains tree 4" ~: Main.contains Main.tree 4 @?= True
       , " contains tree 0" ~: Main.contains Main.tree 0 @?= False
       , " contains tree 7" ~: Main.contains Main.tree 7 @?= False
       , " contains tree 9" ~: Main.contains Main.tree 9 @?= False
       ]]
