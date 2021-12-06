configGhcErrors:
- deprecation
- empty-enumerations
- identities
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
- Use newtype instead of data
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
allowModifying: false
allowRemoving: false
configHlintGroups:
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
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
import Data.List
import Data.Maybe
import Test.QuickCheck

{- Consider the following known data types: -}

data Bit    = O | I                              deriving (Read, Show, Eq)
data Tree a = Leaf a | Node (Tree a) a (Tree a)  deriving (Show, Eq)

{- Your task is to write a function which, for values of type
 - Tree Bit, reverses the effect of 'show', meaning that it takes a
 - string representation and converts it into an actual value of type
 - Tree Bit.
 -}

parse :: String -> Tree Bit
parse = undefined

{- For this you may assume that only valid string representations will
 - be given to you, so you don't have to add any error handling and
 - your function only has to handle the proper conversion, in the
 - sense that 'parse (show t)' returns t, for all t :: Tree Bit.
 -}

test :: Property
test = forAll (elements [1..6]) $ \(Blind h) -> forAll (sizedTree h)
                                $ \t -> parse (show t) == t

main :: IO ()
main = quickCheck test

{- The show function, which has to be "reversed" here, is *not* any
 - manually written function, but a function automatically generated
 - by Haskell (see "deriving (Show, Eq)" at the top).
 -
 - Your function has to, for example, satisfy the following:
 -
 -      parse "Node (Node (Leaf I) O (Leaf I)) I (Leaf O)"
 -   == Node (Node (Leaf I) O (Leaf I)) I (Leaf O)
 -
 - Hint for possible solutions:
 -
 - It's worthwhile to consider using an idea from Task 51,
 - specifically the generalization of decode to decode'. Try to
 - implement parse via a function parse' :: String -> (Tree Bit, String),
 - for which parse' (show t ++ s) == (t,s) should always hold.
 -}

{- The following are just helper definitions for QuickCheck: -}

instance Arbitrary Bit where
  arbitrary = elements [O,I]

sizedTree :: Arbitrary a => Integer -> Gen (Tree a)
sizedTree 0 = fmap Leaf arbitrary
sizedTree n = frequency [ (1, sizedTree 0), (2^n, branching) ]
  where branching = do t1 <- sizedTree (n-1)
                       a  <- arbitrary
                       t2 <- sizedTree (n-1)
                       return (Node t1 a t2)
-------------------------------------
module Test (test) where
import TestHelper (qcWithTimeout)
import qualified Main

test :: [IO ()]
test =
  [qcWithTimeout 5000 Main.test]
