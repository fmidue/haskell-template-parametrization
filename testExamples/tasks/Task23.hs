dat = return "[(\"A\",\"Maybe A | Either A A | Integer | Bool | [Bool] | [Integer]\")]"
gen_value1 = "A" 1 1 False #{seed} #{dat}
gen_value2 = "(A, A)" 1 1 False #{seed}1 #{dat}
gen_value3 = "((A, A), A)" 1 1 False #{seed}2 #{dat}
gen_value4 = "A" 2 3 False #{seed}3 #{dat}
gen_value5 = "(A, A)" 2 3 False #{seed}4 #{dat}
gen_value6 = "(A, (A, A))" 1 3 False #{seed}5 #{dat}
gen_value7 = "A" 3 4 False #{seed}6 #{dat}
-----
configGhcErrors:
- deprecation
- empty-enumerations
- identities
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
configHlintErrors:
- Avoid reverse
- Collapse lambdas
- Evaluate
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant if
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use &&
- Use /=
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use camelCase
- Use drop
- Use elem
- Use even
- Use fst
- Use guards
- Use head
- Use id
- Use if
- Use init
- Use last
- Use left fold instead of right fold
- Use list literal pattern
- Use odd
- Use otherwise
- Use product
- Use right fold instead of left fold
- Use snd
- Use sum
- Use take
- Use ||
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
allowAdding: false
allowModifying: false
allowRemoving: false
configHlintGroups:
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Apply De Morgan law
- Avoid lambda
- Avoid lambda using `infix`
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
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
- Use section
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
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

{- Read up on https://hoogle.haskell.org about the type constructor
 - Either.
 -
 - Then give concrete, different, finite values of the following
 - types. Use every data constructor of Maybe and Either at least
 - once.
 -}

value1 :: #{gen_value1}
value1 = undefined

value2 :: #{gen_value2}
value2 = undefined

value3 :: #{gen_value3}
value3 = undefined

value4 :: #{gen_value4}
value4 = undefined

value5 :: #{gen_value5}
value5 = undefined

value6 :: #{gen_value6}
value6 = undefined

value7 :: #{gen_value7}
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
       , "value1 and value2 are different"
        ~: (Main.value1 /= Main.value2) @?= True
       , " value3 okay?" ~: isDeeplyDefined Main.value3
       , " value4 okay?" ~: isDeeplyDefined Main.value4
       , " value5 okay?" ~: isDeeplyDefined Main.value5
       , " value3, value4, value5 are pairwise different"
        ~: (nub [Main.value3, Main.value4, Main.value5] == [Main.value3, Main.value4, Main.value5]) @?= True
       , " value6 okay?" ~: isDeeplyDefined Main.value6
       , " value7 okay?" ~: isDeeplyDefined Main.value7
       , " value6, value7 different?" ~: (Main.value6 /= Main.value7) @?= True
       , " list of unused data constructors is empty?" ~:
          unusedConstrs
            (concat
              [ constrsUsed Main.value1
              , constrsUsed Main.value2
              , constrsUsed Main.value3
              , constrsUsed Main.value4
              , constrsUsed Main.value5
              , constrsUsed Main.value6
              , constrsUsed Main.value7
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
