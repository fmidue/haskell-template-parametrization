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
module Solution where
import Test.HUnit ((@?=),(~:),runTestTT,Test(TestList))

-- Define, for the following algebraic data type:

data Tree = Leaf Integer | Node Tree Integer Tree  deriving Show

-- three general functions:

midfix, prefix, postfix :: Tree -> [Integer]
midfix  = undefined
prefix  = undefined
postfix = undefined

-- with different traversal strategies, as exemplarily shown here:

test :: [ Test ]
test = -- runTestTT $ TestList
   [ "midfix"  ~: midfix  (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5))
                    @?= [1, 2, 3, 4, 5]
   , "prefix"  ~: prefix  (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5))
                    @?= [4, 2, 1, 3, 5]
   , "postfix" ~: postfix (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5))
                    @?= [1, 3, 2, 5, 4]
   ]
-------------------------------------
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving #-}
module Test (test) where
import Prelude
import Solution (Tree(..))
import qualified Solution
import TestHelper (qcWithTimeout)
import Test.QuickCheck
import Test.HUnit ((~:),Test)

test :: [[ Test ]]
test = [ Solution.test,
   [ " midfix (random trees)"  ~: qcWithTimeout 5000000 (\(t :: Tree) -> Solution.midfix  t == midfix t)
   , " prefix (random trees)"  ~: qcWithTimeout 5000000 (\(t :: Tree) -> Solution.prefix  t == prefix t)
   , " postfix (random trees)" ~: qcWithTimeout 5000000 (\(t :: Tree) -> Solution.postfix t == postfix t)
   ]]

deriving instance Eq Tree

instance Arbitrary Tree where
   arbitrary = sized $ \n -> vectorOf (2*n+1) arbitrary >>= arbitraryUnflatten


arbitraryUnflatten :: [Integer] -> Gen Tree
arbitraryUnflatten [x] = return (Leaf x)
arbitraryUnflatten xs | length xs >= 3 && odd (length xs) = do
   i <- elements $ filter odd [1..length xs-2]
   case splitAt i xs of
     (a,x:b) -> do
       l <- arbitraryUnflatten a
       r <- arbitraryUnflatten b
       return (Node l x r)
     _ -> error "impossible"
arbitraryUnflatten _ = error "Invalid list passed to arbitraryUnflatten"


prefix, postfix, midfix :: Tree -> [Integer]
midfix (Leaf x)     = [x]
midfix (Node l x r) = midfix l ++ [x] ++ midfix r

prefix (Leaf x)     = [x]
prefix (Node l x r) = [x] ++ prefix l ++ prefix r

postfix (Leaf x)     = [x]
postfix (Node l x r) = postfix l ++ postfix r ++ [x]
