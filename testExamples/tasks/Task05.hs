conditionGenerator = withCurrentSeed (elements ["y >= 2 * x", "y > 2 * x"])
conditionGeneratorL = withCurrentSeed (elements ["2*x", "2*x+1"])
operationGenerator = withCurrentSeed (elements ["x + y", "x * y"])
condition = withCurrentSeed (elements ["at least", "more than"])
operation = withCurrentSeed (elements ["sum", "product"])
-----
configGhcErrors:
- deprecation
- empty-enumerations
- identities
- missing-signatures
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
- unused-matches
- unused-pattern-binds
- unused-top-binds
configHlintErrors:
- Avoid reverse
- Collapse lambdas
- Evaluate
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use /=
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use drop
- Use elem
- Use even
- Use fst
- Use head
- Use id
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
allowAdding: false
allowModifying: false
allowRemoving: false
configHlintGroups:
- teaching
- monomorphic
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- unused-local-binds
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Apply De Morgan law
- Avoid lambda
- Avoid lambda using `infix`
- Eta reduce
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
- Use &&
- Use ++
- Use 1
- Use all
- Use and
- Use any
- Use camelCase
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
- Use if
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
- Use ||
configLanguageExtensions:
- NoTemplateHaskell
- TupleSections
----------
module Solution (list) where
import Prelude hiding (($), (!!))

-- Write a "one-line" list comprehension implementing the following verbal
-- description.
--
-- Assume a positive integer constant c is given. Produce a list of all pairs
-- (x,y) of natural numbers such that all of the following hold:
--
--   * x and y are c-digit numbers
--   * y is #{condition} twice as big as x (#{conditionGenerator})
--   * the #{operation} of x and y is also a c-digit number
--
-- A c-digit number is a natural number with exactly c digits.
-- E.g., the set of 2-digit numbers is {10,...,99}
--
-- The constant c could be changed by someone else (but not by you).
-- Your list definition should then still do the right thing.
--
-- No pair (x,y) should occur more than once in the list.

c :: Integer
c = 3

list :: [(Integer,Integer)]
list = undefined
----------
module Test (test) where
import qualified Solution
import Test.QuickCheck
import TestHelper (qcWithTimeout)
import Data.List (sort)
import Test.HUnit ((~:), (@=?), (@?), Test)

test :: [ Test ]
test =
  [ "Does the list contain any elements?"
  ~: (not $ null Solution.list) @? "Your list is empty!"
  , "Does 'take 10 (sort list)' match the sample solution?"
  ~: (take 10 (sort Solution.list) == take 10 list) @?
    (unlines ["take 10 (sort list) should yield " ++ show (take 10 list)
             ,"But your solution yields " ++ show (take 10 $ sort Solution.list)
             ])
  , "Are the bounds for x correct?"
  ~: (99,198) `notElem` Solution.list @? "The list contains the wrong element (99,198)."
  , "Are the bounds for y correct?"
  ~: [ (100,100) `notElem` Solution.list @? "The list contains the wrong element (100,100)."
     , (100,199) `notElem` Solution.list @? "The list contains the wrong element (17,17)."
     ]
  , "Does the list have the right length?"
  ~: (length Solution.list == length list) @?
    (unlines ["The list should have " ++ show (length list) ++ " elements."
             ,"But your solution has " ++ show (length Solution.list) ++ " elements."
             ])
  , " Is the list correct?"
    ~: (sort Solution.list == list) @? "The list is not correct."
  ]

c :: Integer
c = 3

list :: [(Integer,Integer)]
list = [ (x,y) | x <- [10^(c-1)..10^c-1], y <- [#{conditionGeneratorL}..10^c-1], (#{operationGenerator}) < 10^c ]
