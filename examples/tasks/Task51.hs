enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
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
import Test.HUnit
import Data.List (nub)

{- Give concrete, different, finite values of the following types.
 - Use every data constructor of Maybe and Either at least once.
 -}

value1 :: Either Bool [Bool]
value1 = undefined

value2 :: Either Bool [Bool]
value2 = undefined

value3 :: Either Bool [Bool]
value3 = undefined

value4 :: (Either (Integer, Maybe Integer) (Maybe Bool), Maybe Integer)
value4 = undefined

value5 :: Maybe (Either (Maybe Integer, Bool) (Integer, Maybe Bool))
value5 = undefined

value6 :: Maybe (Either (Maybe Integer, Bool) (Integer, Maybe Bool))
value6 = undefined

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "value1, value2, value3 pairwise different" ~:
            (nub [value1, value2, value3] == [value1, value2, value3]) @?= True
          _ <- runTestTT $ "value5, value6 different" ~:
            (value5 /= value6) @?= True
          return ()
----------
module Test (test) where
import qualified #{moduleName}
import Test.HUnit ((~:), (@?=), Test)
import Data.List (nub)
import Data.Data
import qualified Data.Set as S (fromList, toList, (\\))

import TestHelper (isDeeplyDefined)

test :: [[ Test ]]
test = [
       [ " value1 okay?" ~: isDeeplyDefined #{moduleName}.value1
       , " value2 okay?" ~: isDeeplyDefined #{moduleName}.value2
       , " value3 okay?" ~: isDeeplyDefined #{moduleName}.value3
       , " value1, value2, value3 pairwise different?" ~:
         (nub [#{moduleName}.value1, #{moduleName}.value2, #{moduleName}.value3] == [#{moduleName}.value1, #{moduleName}.value2, #{moduleName}.value3]) @?= True
       , " value4 okay?" ~: isDeeplyDefined #{moduleName}.value4
       , " value5 okay?" ~: isDeeplyDefined #{moduleName}.value5
       , " value6 okay?" ~: isDeeplyDefined #{moduleName}.value6
       , " value5, value6 different?" ~: (#{moduleName}.value5 /= #{moduleName}.value6) @?= True
       , " list of unused data constructors is empty?" ~:
          unusedConstrs
            (concat
              [ constrsUsed #{moduleName}.value1
              , constrsUsed #{moduleName}.value2
              , constrsUsed #{moduleName}.value3
              , constrsUsed #{moduleName}.value4
              , constrsUsed #{moduleName}.value5
              , constrsUsed #{moduleName}.value6
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
