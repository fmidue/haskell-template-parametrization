enableWhitespaceWatermarking = return "True"
types = withSeed (shuffle ["A","C","D"]) (#{seed} + 1)
t = return (#{types}!!0)
v = return (#{types}!!1)
w = return (#{types}!!2)
constructors = withSeed (shuffle ["E","F","G","H","I","J","K"]) (#{seed} + 2)
a = return (#{constructors}!!0)
b = return (#{constructors}!!1)
c = return (#{constructors}!!2)
d = return (#{constructors}!!3)
e = return (#{constructors}!!4)
f = return (#{constructors}!!5)
g = return (#{constructors}!!6)
bool1 = withSeed (elements [False, True]) (#{seed} + 3)
bool2 = withSeed (elements [False, True]) (#{seed} + 4)
bool3 = withSeed (elements [False, True]) (#{seed} + 5)
int1 = withSeed (elements [2..8]) (#{seed} + 6)
int2 = withSeed (elements [1..6]) (#{seed} + 7)
int3 = withSeed (elements [3..9]) (#{seed} + 8)
twoOrThree = withSeed (shuffle [0,1]) (#{seed} + 9)
t23 = return (show ["#{t}","#{v}"])
v23 = return (show ["#{d} #{bool1} (#{c} (Left (#{int1}, #{int2})))","#{g} #{int1} [#{bool2}, #{bool3}] (#{c} (Right ()))"])
t2 = return (#{t23}!!(#{twoOrThree}!!0))
v2 = return (#{v23}!!(#{twoOrThree}!!0))
t3 = return (#{t23}!!(#{twoOrThree}!!1))
v3 = return (#{v23}!!(#{twoOrThree}!!1))
----------
# the seed used was: #{seed}
configGhcErrors:
- empty-enumerations
- overflowed-literals
configHlintErrors:
allowAdding: true
allowModifying: false
allowRemoving: false
configHlintGroups:
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
configHlintRules:
configHlintSuggestions:
configLanguageExtensions:
- NoTemplateHaskell
- TupleSections
# configLanguageExtensions - this sets LanguageExtensions for hlint as well
# configHlintSuggestions   - hlint hints to provide
# configHlintErrors        - hlint hints to enforce
# configGhcWarnings        - GHC warnings to provide as hints
# configGhcErrors          - GHC warnings to enforce
----------
module Solution where

-- Give datatype definitions for types A, B, C and D, (just) such that
-- the values given below become valid values of the respective types.
-- Do not introduce any additional datatypes besides A, B, C and D.

value1 :: B
value1 = #{b} (Just (#{e} #{f}))

value2 :: #{t2}
value2 = #{v2}

value3 :: #{t3}
value3 = #{v3}

value4 :: #{w}
value4 = #{a} [#{e} (#{g} #{int3} [] (#{d} False (#{c} (Left (#{int3}, #{int2}))))), #{f}] (#{int1}, True)
-------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Test (test) where
import qualified Solution
import Solution (A(..),B(..),C(..),D(..))
import Test.HUnit ((~:), (@?=), Test)
import Data.List (sort)
import GHC.Generics (Generic)
import Data.Data
import qualified Data.Set as S (Set, fromList, toList, (\\))

deriving instance Eq A
deriving instance Eq B
deriving instance Eq C
deriving instance Eq D

deriving instance Generic A
deriving instance Generic B
deriving instance Generic C
deriving instance Generic D

deriving instance Data A
deriving instance Data B
deriving instance Data C
deriving instance Data D

test :: [[ Test ]]
test = [
       [ " list of unused data constructors is empty?" ~:
          unusedConstrs
            (concat
              [ constrsUsed Solution.value1
              , constrsUsed Solution.value2
              , constrsUsed Solution.value3
              , constrsUsed Solution.value4
              ])
            tuvw
          @?= []
      , " no types other than A, B, C, D used?" ~:
        sort (S.toList $ allConstrs tuvw) == ["E","F","G","H","I","J","K"] @?= True
       ]]

tuvw :: [DataType]
tuvw =
  [ dataTypeOf (undefined :: A)
  , dataTypeOf (undefined :: B)
  , dataTypeOf (undefined :: C)
  , dataTypeOf (undefined :: D)
  ]

constrsUsed :: Data a => a -> [Constr]
constrsUsed x = toConstr x : gmapQl (++) [] constrsUsed x

unusedConstrs :: [Constr] -> [DataType] -> [String]
unusedConstrs cs ds =
 let
   usedConstrs = S.fromList $ map show cs
 in S.toList $ allConstrs ds S.\\ usedConstrs

allConstrs :: [DataType] -> S.Set String
allConstrs ds = S.fromList $ concatMap (map show . dataTypeConstrs) ds
