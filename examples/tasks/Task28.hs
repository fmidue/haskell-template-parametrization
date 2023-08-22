enableWhitespaceWatermarking = return "True"
moduleName = return "Task28"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- name-shadowing

#{commonConfigHlintErrors}
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

allowAdding: false
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
- Eta reduce
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
# - Use newtype instead of data # in general not, since we might want to pose the task also with 'data' and a single data constructor for some type
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
{-# LANGUAGE StandaloneDeriving #-}
module #{moduleName} where
import Test.HUnit
import Data.List (nub)

data T = A Bool | B [Int]
data U = C Bool V | D V
data V = E Int | F (Int, Bool)
data W = G V | H | I U

-- Give concrete, different, finite values of the following types.
-- Use every data constructor of the types defined above at least
-- once. Also, do not reuse any subexpressions.

value1 :: T
value1 = undefined

value2 :: T
value2 = undefined

value3 :: U
value3 = undefined

value4 :: (U, V)
value4 = undefined

value5 :: (V, W)
value5 = undefined

value6 :: (W, U)
value6 = undefined

value7 :: W
value7 = undefined

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "value1 and value2 are different" ~:
            (value1 /= value2) @?= True
          _ <- runTestTT $ "value3, fst value4, snd value6 are pairwise different" ~:
            (nub [value3, fst value4, snd value6] == [value3, fst value4, snd value6]) @?= True
          _ <- runTestTT $ "snd value4 and fst value5 are different" ~:
            (snd value4 /= fst value5) @?= True
          _ <- runTestTT $ "snd value5, fst value6, value7 are pairwise different" ~:
            (nub [snd value5, fst value6, value7] == [snd value5, fst value6, value7]) @?= True
          return ()

deriving instance Eq T
deriving instance Eq U
deriving instance Eq V
deriving instance Eq W
-------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Test (test) where
import qualified #{moduleName}
import #{moduleName} (T,U,V,W)
import Test.HUnit ((~:), (@?=), Test)
import Data.List (nub)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Data
import qualified Data.Set as S (fromList, toList, (\\))

import TestHelper (isDeeplyDefined)

deriving instance Generic T
deriving instance Generic U
deriving instance Generic V
deriving instance Generic W
deriving instance NFData T
deriving instance NFData U
deriving instance NFData V
deriving instance NFData W

deriving instance Data T
deriving instance Data U
deriving instance Data V
deriving instance Data W

test :: [[ Test ]]
test = [
       [ " value1 okay?" ~: isDeeplyDefined #{moduleName}.value1
       , " value2 okay?" ~: isDeeplyDefined #{moduleName}.value2
       , " value1, value2 different?" ~: (#{moduleName}.value1 /= #{moduleName}.value2) @?= True
       , " value3 okay?" ~: isDeeplyDefined #{moduleName}.value3
       , " value4 okay?" ~: isDeeplyDefined #{moduleName}.value4
       , " value5 okay?" ~: isDeeplyDefined #{moduleName}.value5
       , " value6 okay?" ~: isDeeplyDefined #{moduleName}.value6
       , " value7 okay?" ~: isDeeplyDefined #{moduleName}.value7
       , " list of unused data constructors is empty?" ~:
          unusedConstrs
            (concat
              [ constrsUsed #{moduleName}.value1
              , constrsUsed #{moduleName}.value2
              , constrsUsed #{moduleName}.value3
              , constrsUsed #{moduleName}.value4
              , constrsUsed #{moduleName}.value5
              , constrsUsed #{moduleName}.value6
              , constrsUsed #{moduleName}.value7
              ])
            [ dataTypeOf (undefined :: T)
            , dataTypeOf (undefined :: U)
            , dataTypeOf (undefined :: V)
            , dataTypeOf (undefined :: W)
            ]
          @?= []
       , "value3, fst value4, snd value6 are pairwise different" ~:
         (nub [#{moduleName}.value3, fst #{moduleName}.value4, snd #{moduleName}.value6] == [#{moduleName}.value3, fst #{moduleName}.value4, snd #{moduleName}.value6]) @?= True
       , "snd value5, fst value6, value7 are pairwise different" ~:
         (nub [snd #{moduleName}.value5, fst #{moduleName}.value6, #{moduleName}.value7] == [snd #{moduleName}.value5, fst #{moduleName}.value6, #{moduleName}.value7]) @?= True
       ]]

constrsUsed :: Data a => a -> [Constr]
constrsUsed x = toConstr x : gmapQl (++) [] constrsUsed x

unusedConstrs :: [Constr] -> [DataType] -> [String]
unusedConstrs cs ds =
 let
   allConstrs = S.fromList $ concatMap (map show . dataTypeConstrs) ds
   usedConstrs = S.fromList $ map show cs
 in S.toList $ allConstrs S.\\ usedConstrs
