enableWhitespaceWatermarking = return "True"
wordingWatermark = withCurrentSeed (elements ["are still", "remain"])
start = withSeed (elements [0,1]) (#{seed} + 1)
----------
# the seed used was: #{seed}
configGhcErrors:
- empty-enumerations
- overflowed-literals
configHlintErrors:
allowAdding: false
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

-- Every list comprehension can be expressed via map, filter, and
-- concat.
--
-- Here is a concrete list comprehension:

original :: [ (Integer, Integer) ]
original = [ (u,v) | u <- [#{start}..100], v <- [1..u], u `mod` v > 0 ]

-- Your task is to implement the same without using list
-- comprehensions (though range expressions #{wordingWatermark} allowed).
--
-- A rough idea is already given here. Complete that definition
-- by replacing the occurrences of 'undefined' appropriately.
--
-- Do not use any additional top-level or local (let, where)
-- definitions.

alternative :: [ (Integer, Integer) ]
alternative = concat (map (\u -> map (\v -> undefined) undefined) undefined)

-- By executing 'main' below, you can partially test your solution
-- before uploading it.

main :: IO ()
main = do putStrLn "Checking prefixes:"
          quickCheck
           (
            forAllShrink (growingElements [1 .. length original]) shrink
              (\n -> take n original == take n alternative)
           )
----------
module Test (test) where
import qualified Main
import Test.HUnit ((~:), (@?=), Test)

test :: [ Test ]
test = [ " original == alternative" ~:
         (Main.original == Main.alternative) @?= True
       ]
