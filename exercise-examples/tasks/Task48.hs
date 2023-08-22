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
import Prelude hiding ((!!))

{- Implement insertion sort with 'foldr', using a helper function
 - 'insert' (which doesn't necessarily use 'foldr'):
 -}

insert :: Ord a => a -> [a] -> [a]
insert = undefined

{- Here 'insert' should insert an element in the appropriate place of
 - an already sorted list.
 -}

example :: Bool
example = insert 4 [1, 3, 6, 7] == [1, 3, 4, 6, 7]


insertionSort :: Ord a => [a] -> [a]
insertionSort = undefined
--------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Test (test) where
import Prelude
import qualified #{moduleName}
import Data.List (insert)
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:),(@?=), Test)
import Test.QuickCheck (Arbitrary(..), orderedList, suchThat)
import Data.List (sort)
import Control.Applicative ((<$>))
import TestHarness

test :: [[ Test ]]
test = [
       [ " example" ~:
            qcWithTimeout 500000 #{moduleName}.example
       , " insert (random inputs)" ~:
            qcWithTimeout 500000 $ \(x :: Int) (L xs) -> #{moduleName}.insert x xs == insert x xs
       , " insertionSort (random inputs)" ~:
            qcWithTimeout 500000 $ \(xs :: [Int]) -> #{moduleName}.insertionSort xs == sort xs
       , " 'insertionSort' uses 'insert'?" ~:
           syntaxCheck $ \modul ->
              contains (ident "insert") (findTopLevelDeclsOf "insertionSort" modul) @?= True
       , " 'insertionSort' uses 'foldr'?" ~:
           syntaxCheck $ \modul ->
              contains (ident "foldr") (findTopLevelDeclsOf "insertionSort" modul) @?= True
       ]]

newtype L a = L [a]
instance (Arbitrary a, Ord a) => Arbitrary (L a) where
   arbitrary = L <$> orderedList
instance Show a => Show (L a) where
   show (L xs) = show xs
