enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- name-shadowing

#{commonConfigHlintErrors}
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
- Use odd
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds

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
- "Use :"
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
# - Use foldr # might otherwise reveal the solution?
- Use fromMaybe
- Use infix
# - Use isJust
# - Use isNothing
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
# - Use null
- Use or
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Test.QuickCheck

{- Here is a recursive function on lists of integers. It is not really
 - important what it does and why, but it obviously does it by
 - structural recursion:
 -}

original :: [Integer] -> Integer
original [] = 0
original (x:xs) | x < 20    = 5 * x - 3 + original xs
                | otherwise = original xs

{- The 'foldr' function shown in the lecture was said to exactly
 - capture structural recursion on lists. So it should be possible to
 - express the above function as an application of 'foldr'. That is
 - your task. Replace the two occurrences of 'undefined' in the
 - following definition, such that 'original' and 'alternative'
 - compute the same mathematical function.
 -
 - Do not use any additional top-level or local (let, where)
 - definitions.
 -}

alternative :: [Integer] -> Integer
alternative = foldr undefined undefined

-- The obvious test suite:
main :: IO ()
main = quickCheck $ \list -> original list == alternative list
----------
module Test (test) where
import qualified #{moduleName}
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), Test)

test :: [ Test ]
test =
  [ " Test with random inputs"
    ~: qcWithTimeout 500000 $ \list -> #{moduleName}.alternative list == #{moduleName}.original list
  ]
