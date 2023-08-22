enableWhitespaceWatermarking = return "True"
pairTypes = withCurrentSeed (elements ["Bool, [Integer]", "[Integer], Bool"])
pairValues = withCurrentSeed (elements ["False, []", "[], False"])
wordingWatermark = withSeed (elements ["Maybe and Either", "Either and Maybe"]) (#{seed} + 1)
----------
# the seed used was: #{seed}
configGhcErrors:
- empty-enumerations
- overflowed-literals
configHlintErrors:
allowAdding: false
allowModifying: false
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
import Test.HUnit
import Data.List (nub)

-- Give concrete, different, finite values of the following types.
-- Use each data constructor of #{wordingWatermark} at least once.

value1 :: Either Bool (#{pairTypes})
value1 = undefined

value2 :: Either Bool (#{pairTypes})
value2 = undefined

value3 :: (Either (Maybe Bool) (Integer, Maybe Integer), Maybe Integer)
value3 = undefined

value4 :: Maybe (Either Integer (Maybe Bool, Integer, Maybe Bool))
value4 = undefined

value5 :: Maybe (Either Integer (Maybe Bool, Integer, Maybe Bool))
value5 = undefined

value6 :: Maybe (Either Integer (Maybe Bool, Integer, Maybe Bool))
value6 = undefined

-- A very simple test suite:

main :: IO ()
main = do _ <- runTestTT $ "not value1, value2 equal" ~:
            (value1 == value2) @?= False
          _ <- runTestTT $ "value4, value5, value6 pairwise different" ~:
            (nub [value4, value5, value6] == [value4, value5, value6]) @?= True
          return ()
----------
module Test (test) where
import qualified Main
import Test.HUnit ((~:), (@?=), Test)
import Data.List (nub)
import Data.Data
import qualified Data.Set as S (fromList, toList, (\\))

import TestHelper (isDeeplyDefined)

test :: [[ Test ]]
test = [
       [ " value1 okay?" ~: isDeeplyDefined Main.value1
       , " value2 okay?" ~: isDeeplyDefined Main.value2
       , " value1, value2 different?" ~: (Main.value1 /= Main.value2) @?= True
       , " value3 okay?" ~: isDeeplyDefined Main.value3
       , " value4 okay?" ~: isDeeplyDefined Main.value4
       , " value5 okay?" ~: isDeeplyDefined Main.value5
       , " value6 okay?" ~: isDeeplyDefined Main.value6
       , " value4, value5, value6 pairwise different?" ~:
         (nub [Main.value4, Main.value5, Main.value6] == [Main.value4, Main.value5, Main.value6]) @?= True
       , " list of unused data constructors is empty?" ~:
          unusedConstrs
            (concat
              [ constrsUsed Main.value1
              , constrsUsed Main.value2
              , constrsUsed Main.value3
              , constrsUsed Main.value4
              , constrsUsed Main.value5
              , constrsUsed Main.value6
              ])
            [ dataTypeOf (undefined :: Maybe ())
            , dataTypeOf (undefined :: Either () ())
            ]
          @?= []
       ]]

constrsUsed :: Data a => a -> [Constr]
constrsUsed x = toConstr x : gmapQl (++) [] constrsUsed x

unusedConstrs :: [Constr] -> [DataType] -> [String]
unusedConstrs cs ds =
  let
    allConstrs = S.fromList $ concatMap (map show . dataTypeConstrs) ds
    usedConstrs = S.fromList $ map show cs
  in S.toList $ allConstrs S.\\ usedConstrs
