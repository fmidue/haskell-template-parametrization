enableWhitespaceWatermarking = return "True"
moduleName = return "Task12"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
# - incomplete-patterns # might reveal list patterns
# - incomplete-uni-patterns # might reveal list patterns
- missing-signatures
- name-shadowing
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
# - Use guards # not necessarily
- Use if
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate
- Use ||

allowAdding: false
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- unused-local-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
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
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
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
import qualified #{moduleName}
import TestHelper (qcWithTimeout)
import TestHarness
import Test.HUnit (Test, (@=?), (@?), (~:))

test :: [[ Test ]]
test =
  [[
    " Testing 'notElem 4 [1..10]'"
    ~: False @=? #{moduleName}.notElem 4 [1..10],
    " Testing 'notElem 7 [9,8,7,6,5]'"
    ~: False @=? #{moduleName}.notElem 7 [9,8,7,6,5],
    " Testing 'notElem 0 []'"
    ~: True @=? #{moduleName}.notElem 0 [],
    " Testing 'notElem 4 [9,8,7,6,5]'"
    ~: True @=? #{moduleName}.notElem 4 [9,8,7,6,5],
    "Testing with random values:"
    ~: qcWithTimeout 5000 $ \v xs -> #{moduleName}.notElem v xs == Prelude.notElem v xs
  ]]
