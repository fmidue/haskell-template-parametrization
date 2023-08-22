enableWhitespaceWatermarking = return "True"
moduleName = return "Task23"
hoogle = return "https://hoogle.haskell.org"
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

{- Read up at #{hoogle} about the type constructor
 - Either.
 -
 - Then give concrete, different, finite values of the following
 - types. Use every data constructor of Maybe and Either at least
 - once.
 -}

value1 :: Either Bool [(Integer,Bool)]
value1 = undefined

value2 :: Either Bool [(Integer,Bool)]
value2 = undefined

value3 :: (Maybe (Either (Integer, Maybe Integer) Bool), Maybe Integer)
value3 = undefined

value4 :: (Maybe (Either (Integer, Maybe Integer) Bool), Maybe Integer)
value4 = undefined

value5 :: (Maybe (Either (Integer, Maybe Integer) Bool), Maybe Integer)
value5 = undefined

value6 :: Either (Maybe Integer, Bool) (Maybe (Integer, Maybe Bool))
value6 = undefined

value7 :: Either (Maybe Integer, Bool) (Maybe (Integer, Maybe Bool))
value7 = undefined

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "value1 and value2 are different" ~:
            (value1 /= value2) @?= True
          _ <- runTestTT $ "value3, value4, value5 are pairwise different" ~:
            (nub [value3, value4, value5] == [value3, value4, value5]) @?= True
          _ <- runTestTT $ "value6 and value7 are different" ~:
            (value6 /= value7) @?= True
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
       , "value1 and value2 are different"
        ~: (#{moduleName}.value1 /= #{moduleName}.value2) @?= True
       , " value3 okay?" ~: isDeeplyDefined #{moduleName}.value3
       , " value4 okay?" ~: isDeeplyDefined #{moduleName}.value4
       , " value5 okay?" ~: isDeeplyDefined #{moduleName}.value5
       , " value3, value4, value5 are pairwise different"
        ~: (nub [#{moduleName}.value3, #{moduleName}.value4, #{moduleName}.value5] == [#{moduleName}.value3, #{moduleName}.value4, #{moduleName}.value5]) @?= True
       , " value6 okay?" ~: isDeeplyDefined #{moduleName}.value6
       , " value7 okay?" ~: isDeeplyDefined #{moduleName}.value7
       , " value6, value7 different?" ~: (#{moduleName}.value6 /= #{moduleName}.value7) @?= True
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
