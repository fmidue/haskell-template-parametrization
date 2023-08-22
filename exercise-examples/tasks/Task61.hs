enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use ++
- Use 1
- "Use :"
- Use all
- Use and
- Use any
- Use camelCase
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use even
- Use find
- Use floor
- Use foldr
- Use fromMaybe
- Use guards
- Use if
- Use infix
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
- Use odd
- Use or
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tuple-section
# - Use uncurry
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings: []

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Use foldl
- Use tail

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
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
import #{moduleName} (Tree(..))
import qualified #{moduleName}
import TestHelper (qcWithTimeout)
import Test.QuickCheck
import Test.HUnit ((~:),Test)

test :: [[ Test ]]
test = [ #{moduleName}.test,
   [ " midfix (random trees)"  ~: qcWithTimeout 5000000 (\(t :: Tree) -> #{moduleName}.midfix  t == midfix t)
   , " prefix (random trees)"  ~: qcWithTimeout 5000000 (\(t :: Tree) -> #{moduleName}.prefix  t == prefix t)
   , " postfix (random trees)" ~: qcWithTimeout 5000000 (\(t :: Tree) -> #{moduleName}.postfix t == postfix t)
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
