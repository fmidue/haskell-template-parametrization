enableWhitespaceWatermarking = return "True"
moduleName = return "Task18"
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
import Prelude hiding (($), (!!), take, drop, splitAt, elem, notElem, length, reverse, last, foldl, foldr)
import Test.QuickCheck

{-
Write a function 'fibSum' for the given type signature that computes

1 * x1 + 2 * x2 + 3 * x3 + 4 * x5 + 5 * x8 + ... ,

for a given list [x1,x2,x3,x4,x5,x6,x7,x8,...,xn].

That is, it computes the sum of j * xi where i <= n is the j-th
fibonacci number. You can make use of the isFib function given below.

(For this task, the fibonacci numbers start with 1 and 2, see fibList
below.)

Do not use any form of recursion to solve the task!
-}

-- To convince yourself that the following definition is working, you
-- might want to copy it into a different file, load that file into
-- ghci, and run something like 'take 100 fibList'.
fibList :: [Integer]
fibList = 1 : 2 : [ a + b | (a, b) <- zip fibList (tail fibList) ]

-- The following function computes the smallest fibonacci number that
-- is greater or equal to n.
nextFib :: Integer -> Integer
nextFib n = head [ m | m <- fibList, m >= n ]

-- The following function is what you probably want to use.
isFib :: Integer -> Bool
isFib n = n == nextFib n

fibSum :: [Integer] -> Integer
fibSum = undefined

main :: IO ()
main = do
  putStrLn "Two simple unit tests"
  quickCheck (fibSum [1 .. 20] == 1 + 2 * 2 + 3 * 3 + 4 * 5 + 5 * 8 + 6 * 13)
  quickCheck (fibSum [20, 19 .. 1] == 1 * 20 + 2 * 19 + 3 * 18 + 4 * 16 + 5 * 13 + 6 * 8)
----------
module Test (test) where
import qualified #{moduleName}
import Test.QuickCheck
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), (@?=), Test)

test :: [ Test ]
test =
  [
    "Testing with random values:"
    ~: qcWithTimeout 10000 $ \xs -> #{moduleName}.fibSum xs == Test.fibSum xs
  ]

fibSum :: [Integer] -> Integer
fibSum xs = sum [ i * v | (i,v) <- zip [1..] fibs ]
  where fibs = map snd $ filter (#{moduleName}.isFib . fst) (zip [1..] xs)
