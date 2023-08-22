enableWhitespaceWatermarking = return "True"
wordingWatermark = withCurrentSeed (elements ["variants", "versions"])
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
import Prelude hiding (elem, notElem)
import Test.QuickCheck

-- Consider the following definitions:

noDuplicates :: [Integer] -> [Integer]
noDuplicates [] = []
noDuplicates (x:xs) | notElem x ys = x:ys
                    | otherwise    = ys
   where ys = noDuplicates xs

notElem :: Integer -> [Integer] -> Bool
notElem _ []             = True
notElem x (y:_) | x == y = False
notElem x (_:ys)         = notElem x ys

-- Write semantically equivalent #{wordingWatermark} of noDuplicates and notElem
-- using foldr.

noDuplicates' :: [Integer] -> [Integer]
noDuplicates' = foldr undefined undefined

notElem' :: Integer -> [Integer] -> Bool
notElem' x = foldr undefined undefined

-- By executing 'main' below, you can test your solution before
-- uploading it.

main :: IO ()
main = do quickCheck $ \xs -> noDuplicates xs == noDuplicates' xs
          quickCheck $ \x xs -> notElem x xs == notElem' x xs
-------------------------------------
module Test (test) where
import qualified Main
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), Test)

test :: [[Test]]
test = [
  [ "noDuplicates'"  ~: qcWithTimeout 5000 (\xs -> Main.noDuplicates xs == Main.noDuplicates' xs)
  , "notElem'" ~: qcWithTimeout 5000 (\x xs -> Main.notElem x xs == Main.notElem' x xs)
  ]]
