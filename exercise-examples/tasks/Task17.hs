enableWhitespaceWatermarking = return "True"
moduleName = return "Task17"
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

allowAdding: true
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
- Use notElem
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
import Prelude hiding (($), (!!), take, drop, splitAt)
import Test.QuickCheck

{-
Write a function 'hasEvenEvens' for the given type signature which returns
whether a list contains an even amount of even numbers.

By executing 'main' below, you can test your solution
before uploading it.
-}

hasEvenEvens :: [Integer] -> Bool
hasEvenEvens = undefined

main :: IO ()
main = do putStrLn "Empty list [] has an even amount of even numbers:"
          quickCheck (hasEvenEvens [])
          putStrLn "Singleton list [x] cannot have an even amount of even numbers if x is even:"
          quickCheck (\x -> even x ==> not (hasEvenEvens [x]))
          putStrLn "Singleton list [x] has an even amount of even numbers if x is odd:"
          quickCheck (\x -> odd x ==> hasEvenEvens [x])
          putStrLn "A combined list contains an even amount of even numbers exactly if the separate lists both have even or both have odd many even numbers:"
          quickCheck (\xs ys -> hasEvenEvens (xs ++ ys) == (hasEvenEvens xs == hasEvenEvens ys))
----------
{-# LANGUAGE ScopedTypeVariables #-}
module Test (test) where
import qualified #{moduleName}
import Prelude hiding (mod)
import Control.Monad (unless)
import Language.Haskell.Exts.Syntax (Decl (FunBind))
import TestHelper (qcWithTimeout)
import TestHarness
import Test.HUnit (Test, (@=?), (@?), (~:), assertFailure, assertBool)
import Test.QuickCheck

test :: [ Test ]
test =
  [
      "Empty list [] has an even amount of even numbers:"
      ~: qcWithTimeout 5000 (hasEvenEvens [])
    , "Singleton list [x] cannot have an even amount of even numbers if x is even:"
      ~: qcWithTimeout 5000 (\x -> even x ==> not (hasEvenEvens [x]))
    , "Singleton list [x] has an even amount of even numbers if x is odd:"
      ~: qcWithTimeout 5000 (\x -> odd x ==> hasEvenEvens [x])
    , "A combined list contains an even amount of even numbers exactly if the separate lists both have even or both have odd many even numbers:"
      ~: qcWithTimeout 10000 (\xs ys -> hasEvenEvens (xs ++ ys) == (hasEvenEvens xs == hasEvenEvens ys))
    , "Testing with random values:"
      ~: qcWithTimeout 5000 (\xs -> #{moduleName}.hasEvenEvens xs == Test.hasEvenEvens xs)
  ]

hasEvenEvens :: [Integer] -> Bool
hasEvenEvens = even . length . filter even
