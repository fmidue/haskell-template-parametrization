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
# - Use newtype instead of data
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
allowModifying: true
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
import qualified #{moduleName}
import Test.QuickCheck
import Test.HUnit ((~:), (@?=),Test)

test :: [[ Test ]]
test = [
       [ " contains tree 3" ~: #{moduleName}.contains #{moduleName}.tree 3 @?= False
       , " contains tree 8" ~: #{moduleName}.contains #{moduleName}.tree 8 @?= True
       , " contains tree 5" ~: #{moduleName}.contains #{moduleName}.tree 5 @?= True
       , " contains tree 1" ~: #{moduleName}.contains #{moduleName}.tree 1 @?= True
       , " contains tree 6" ~: #{moduleName}.contains #{moduleName}.tree 6 @?= True
       , " contains tree 2" ~: #{moduleName}.contains #{moduleName}.tree 2 @?= True
       , " contains tree 4" ~: #{moduleName}.contains #{moduleName}.tree 4 @?= True
       , " contains tree 0" ~: #{moduleName}.contains #{moduleName}.tree 0 @?= False
       , " contains tree 7" ~: #{moduleName}.contains #{moduleName}.tree 7 @?= False
       , " contains tree 9" ~: #{moduleName}.contains #{moduleName}.tree 9 @?= False
       ]]
