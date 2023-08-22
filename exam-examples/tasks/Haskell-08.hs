enableWhitespaceWatermarking = return "True"
wordingWatermark = withCurrentSeed (elements ["biggest", "highest"])
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
import System.IO

-- Write an IO program that reads in three integers (negative integers
-- too) and prints out the #{wordingWatermark} of them. This behavior is repeated
-- until the first new number read is 0. The program then immediately
-- terminates (not even reading a new second or third number) after
-- printing a count of completed iterations (that is the count of
-- maximums printed).
--
-- You can add additional information to both the output of the
-- maximum/comparison results as well as the final output. Moreover,
-- you might want to add additional outputs to indicate throughout
-- what the user has to do next.

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          undefined
----------
module Test (test) where
import Test.HUnit (Test)

test :: [ Test ]
test = [ ]
