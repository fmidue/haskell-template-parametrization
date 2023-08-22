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
import Prelude hiding ((!!), head, tail, last, init, take, drop, takeWhile, dropWhile)

{- Write a variant of the 'filter' function, named 'takeWhile'.
 - Unlike 'filter', which reduces a given list to all elements of it
 - that satisfy a given predicate, 'takeWhile' should produce a list
 - of all elements of a given list up to (but not including) the first
 - element where the predicate is not satisfied; so for example:
 - takeWhile (<5) [1,2,5,4,3,6] should result in [1,2].
 -}

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined
-------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Test where
import qualified #{moduleName} (takeWhile)
import Prelude
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), Test)

test :: [Test]
test =
  [" takeWhile odd (random entries)" ~:
     qcWithTimeout 500000 $ \(xs :: [Int]) ->
     #{moduleName}.takeWhile odd xs == Prelude.takeWhile odd xs]
