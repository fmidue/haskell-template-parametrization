configGhcErrors:
- deprecation
- empty-enumerations
- identities
# - incomplete-patterns # might reveal list patterns
# - incomplete-uni-patterns # might reveal list patterns
- missing-signatures
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
- unused-matches
- unused-pattern-binds
configHlintErrors:
- Avoid reverse
- Collapse lambdas
- Eta reduce
- Evaluate
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
- Use /=
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use camelCase
- Use drop
- Use elem
- Use even
- Use fst
# - Use guards # not necessarily
- Use head
- Use id
- Use if
- Use init
# - Use isJust
# - Use isNothing
- Use last
- Use left fold instead of right fold
- Use list literal pattern
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use otherwise
- Use product
- Use replicate
- Use right fold instead of left fold
- Use snd
- Use sum
- Use take
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
- unused-local-binds
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Apply De Morgan law
- Avoid lambda
- Avoid lambda using `infix`
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
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
- Use section
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
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
import qualified Main
import Test.QuickCheck
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), (@?=), Test)

test :: [ Test ]
test =
  [
    "Testing with random values:"
    ~: qcWithTimeout 10000 $ \xs -> Main.fibSum xs == Test.fibSum xs
  ]

fibSum :: [Integer] -> Integer
fibSum xs = sum [ i * v | (i,v) <- zip [1..] fibs ]
  where fibs = map snd $ filter (Main.isFib . fst) (zip [1..] xs)
