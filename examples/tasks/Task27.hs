enableWhitespaceWatermarking = return "True"
moduleName = return "Task27"
slide = return "151"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
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

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
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
- "Use :"
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
import Prelude hiding ((!!))
import Test.QuickCheck
import Data.Maybe -- contains useful functions like isNothing, isJust,
                  -- and fromJust; but do also consider slide #{slide}

{- Consider the following two enumeration types
 - (but ignore the 'deriving' stuff):
 -}

data Animal = Cat | Dog | Bird
  deriving (Eq, Show, Enum, Bounded)

data Bit = O | I
  deriving (Eq, Show, Enum, Bounded)

{- Assume we want to encode and decode values of type [Animal] to and
 - from bit sequences. Essentially, we want to implement serialisation
 - and deserialisation functionality.
 -
 - So, write an *injective* function:
 -}

encode :: [Animal] -> [Bit]
encode = undefined

{- and another function: -}

decode :: [Bit] -> Maybe [Animal]
decode = undefined

{- such that:
 -
 - a) for every list of type [Bit] that can be produced by 'encode',
 -    the function 'decode' returns the value 'Just l', where l is
 -    exactly the original list before applying 'encode', and
 -
 - b) for every list of type [Bit] for which no corresponding original
 -    list of type [Animal] exists, the function 'decode' returns the
 -    value 'Nothing'.
 -
 - In other words, the general properties corresponding to the
 - following two tests should hold:
 -}

main :: IO ()
main = do quickCheck $ \v -> decode (encode v) == Just v
          quickCheck $ \c -> let mv = decode c
                             in isJust mv ==> encode (fromJust mv) == c

{- Also, for the purposes of serialisation, it is wise to use a
 - space efficient encoding, so do not create unnecessarily
 - long Bit-lists for given Animal-lists in general.
 -}

{- The following definitions are only needed to help QuickCheck.
 - You can ignore them.
 -}

instance Arbitrary Animal where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Bit where
  arbitrary = elements [minBound .. maxBound]
----------
module Test (test) where
import qualified #{moduleName}
import Test.QuickCheck
import Test.HUnit ((~:), (@?=), Test, Assertion, assertFailure, assertBool)
import Data.Maybe
import Data.List (permutations)

import TestHelper (qcWithTimeoutAndRuns)

test :: [ Test ]
test =
  [ " decoding after encoding" ~: qcWithTimeoutAndRuns 5000 80 test1
  , " encoding after decoding" ~:
    sequence_ [ assertBool (" ... on " ++ show c) (test2 (c, d)) | (c, d) <- instances ]
  , " is space efficient?" ~:
    [ " max two Bits used per element?" ~:
      all (<= 2) [size1, size2, size3] @?= True
    , " less than two Bits used for some element?" ~:
      any (< 2) [size1, size2, size3] @?= True
    , " for random lists" ~: -- not completely random
      qcWithTimeoutAndRuns 5000 100 $ forAll nonEmptyBalancedList $ \xs ->
        counterexample ("expected an encoding of size less than " ++ show (length xs * 2)) $
        length (#{moduleName}.encode xs) < length xs * 2
    ]
  ]
  where instances = take 1000 $ filter (isJust . snd)
                              $ map (\c -> (c, #{moduleName}.decode c))
                              $ concatMap cases [(0::Integer) ..]
        cases 0 = [[]]
        cases l = concat [[#{moduleName}.O : bs, #{moduleName}.I : bs] | bs <- cases (l-1)]

test1 :: [#{moduleName}.Animal] -> Bool
test1 v = #{moduleName}.decode (#{moduleName}.encode v) == Just v

test2 :: ([#{moduleName}.Bit], Maybe [#{moduleName}.Animal]) -> Bool
test2 (c, mv) = #{moduleName}.encode (fromJust mv) == c
  -- mv = #{moduleName}.decode c, and isJust mv is known

size1, size2, size3 :: Int
size1 = length (#{moduleName}.encode [#{moduleName}.Cat])
size2 = length (#{moduleName}.encode [#{moduleName}.Dog])
size3 = length (#{moduleName}.encode [#{moduleName}.Bird])

nonEmptyBalancedList :: Gen [#{moduleName}.Animal]
nonEmptyBalancedList =
  concat <$>
    listOf1 (elements $ permutations [#{moduleName}.Cat,#{moduleName}.Dog,#{moduleName}.Bird])
