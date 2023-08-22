enableWhitespaceWatermarking = return "True"
wordingWatermark = withCurrentSeed (elements ["such", "so"])
predicate = withSeed (elements ["even x", "odd x "]) (#{seed} + 1)
factor = withSeed (elements [2, 3]) (#{seed} + 2)
----------
# the seed used was: #{seed}
configGhcErrors:
- empty-enumerations
- overflowed-literals
configHlintErrors:
allowAdding: true
allowModifying: true
allowRemoving: false
configHlintGroups:
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
configHlintRules:
configHlintSuggestions:
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
import Test.QuickCheck

-- Here is a recursive function on lists of integers:

original :: [Integer] -> Integer
original [] = 1
original (x:xs) | #{predicate}    = x - #{factor} * original xs
                | otherwise = original xs

-- Your task is to express the above function as an application of
-- 'foldr'. Just replace the two occurrences of 'undefined' in the
-- following definition, #{wordingWatermark} that 'original' and 'alternative'
-- compute the same mathematical function.

alternative :: [Integer] -> Integer
alternative = foldr undefined undefined

-- The obvious test suite:

main :: IO ()
main = quickCheck $ \l -> original l == alternative l
----------
module Test (test) where
import qualified Main
import TestHelper (qcWithTimeoutAndRuns)
import Test.HUnit ((~:), Test)

test :: [ Test ]
test =
  [ " Test with random inputs"
    ~: qcWithTimeoutAndRuns 5000 100 $ \list -> Main.alternative list == Main.original list
  ]
