enableWhitespaceWatermarking = return "True"
moduleName = return "Task11"
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
import Prelude hiding (($), (!!), tail, init, take, drop, foldr, foldl, sum)
import Test.QuickCheck

{-
 - Write a function 'sum' which computes the sum of a (finite)
 - list of numbers, so that for example: sum [2,5,3] == 10.
 -
 - You should not use pattern-matching on binary list constructors
 - here, even if you happen to already know this concept. Instead, use
 - a divide-and-conquer approach. That is, we expect to see something
 - like 'splitAt (length list `div` 2) list' in your solution.
 -
 - Use exactly one equation (maybe with guards) to define 'sum'. If
 - you feel a need to use 'where', use 'let' instead.
 -}

sum :: [Integer] -> Integer
sum xs = undefined

main :: IO ()
main = do putStrLn "If your code does not even satisfy this, there is a problem:"
          quickCheck (sum [] == 0)
----------
module Test (test) where
import qualified #{moduleName}
import Test.QuickCheck
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), (@?=), Test)

test :: [ Test ]
test =
  [
    " Test with empty list"
    ~: #{moduleName}.sum [] @?= 0,
    " Test with random inputs"
    ~: qcWithTimeout 5000000 $ \ns -> not (null ns)
                   ==> #{moduleName}.sum ns == sum ns
  ]
