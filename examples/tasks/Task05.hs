enableWhitespaceWatermarking = return "True"
moduleName = return "Task05"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- missing-signatures
- name-shadowing
- unused-matches
- unused-pattern-binds
- unused-top-binds

#{commonConfigHlintErrors}
- Redundant bracket
- Use even
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate

allowAdding: false
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- unused-local-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
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
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
- Use ||

#{commonConfigLanguageExtensions}
----------
module #{moduleName} (list) where
import Prelude hiding (($), (!!))

-- Note that this is the first non-CodeWorld-task.
--
-- Write a "one-line" list comprehension implementing the following verbal
-- description.
--
-- Assume a positive integer constant c is given. Produce a list of all pairs
-- (x,y) of natural numbers such that all of the following hold:
--
--   * x and y are c-digit numbers
--   * y is at least twice as big as x
--   * the sum of x and y is also a c-digit number
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
import qualified #{moduleName}
import Test.QuickCheck
import TestHelper (qcWithTimeout)
import Data.List (sort)
import Test.HUnit ((~:), (@=?), (@?), Test)

test :: [ Test ]
test =
  [ "Does the list contain any elements?"
  ~: (not $ null #{moduleName}.list) @? "Your list is empty!"
  , "Does 'take 10 (sort list)' match the sample solution?"
  ~: (take 10 (sort #{moduleName}.list) == take 10 list) @?
    (unlines ["take 10 (sort list) should yield " ++ show (take 10 list)
             ,"But your solution yields " ++ show (take 10 $ sort #{moduleName}.list)
             ])
  , "Are the bounds for x correct?"
  ~: (99,198) `notElem` #{moduleName}.list @? "The list contains the wrong element (99,198)."
  , "Are the bounds for y correct?"
  ~: [ (100,100) `notElem` #{moduleName}.list @? "The list contains the wrong element (100,100)."
     , (100,199) `notElem` #{moduleName}.list @? "The list contains the wrong element (17,17)."
     ]
  , "Does the list have the right length?"
  ~: (length #{moduleName}.list == length list) @?
    (unlines ["The list should have " ++ show (length list) ++ " elements."
             ,"But your solution has " ++ show (length #{moduleName}.list) ++ " elements."
             ])
  , " Is the list correct?"
    ~: (sort #{moduleName}.list == list) @? "The list is not correct."
  ]

c :: Integer
c = 3

list :: [(Integer,Integer)]
list = [ (x,y) | x <- [10^(c-1)..10^c-1], y <- [2*x..10^c-1], x + y < 10^c ]
