enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
slide = return "151"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use ++
- Use 1
- "Use :"
- Use all
- Use and
- Use any
- Use camelCase
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use even
- Use find
- Use floor
- Use foldr
- Use fromMaybe
- Use guards
- Use if
- Use infix
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
- Use odd
- Use or
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tuple-section
# - Use uncurry
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings: []

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Use foldl
- Use tail

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Prelude hiding ((!!))
import Test.QuickCheck
import Data.Maybe -- contains useful functions like isNothing, isJust,
                  -- and fromJust; but do also consider slide #{slide}

{- Consider the following enumeration type
 - (but ignore the 'deriving' stuff):
 -}

data Bit = O | I
  deriving (Eq, Show, Enum, Bounded)

{- Assume we want to encode and decode values of type [[Bool]] to and
 - from bit sequences. Essentially, we want to implement serialisation
 - and deserialisation functionality.
 -
 - So, write an *injective* function:
 -}

encode :: [[Bool]] -> [Bit]
encode = undefined

{- and another function: -}

decode :: [Bit] -> Maybe [[Bool]]
decode = undefined

{- such that:
 -
 - a) for every list of type [Bit] that can be produced by 'encode',
 -    the function 'decode' returns the value 'Just l', where l is
 -    exactly the original list before applying 'encode', and
 -
 - b) for every list of type [Bit] for which no corresponding original
 -    list of type [[Bool]] exists, the function 'decode' returns
 -    the value 'Nothing'.
 -
 - In other words, the general properties corresponding to the
 - following two tests should hold:
 -}

main :: IO ()
main = do quickCheck $ \v -> decode (encode v) == Just v
          quickCheck $ \c -> let mv = decode c
                             in isJust mv ==> encode (fromJust mv) == c

{- The following definition is only needed to help QuickCheck.
 - You can ignore it.
 -}

instance Arbitrary Bit where
  arbitrary = elements [minBound .. maxBound]
----------
module Test (test) where
import qualified #{moduleName}
import Test.QuickCheck
import Test.HUnit ((~:), Test, Assertion, assertFailure, assertBool)
import Data.Maybe

import TestHelper (qcWithTimeoutAndRuns)

test :: [ Test ]
test =
  [ " decoding after encoding" ~: qcWithTimeoutAndRuns 5000000 250 test1
  , " encoding after decoding" ~:
    sequence_ [ assertBool (" ... on " ++ show c) (test2 (c, d)) | (c, d) <- instances ] ]
  where instances = take 250 $ filter (isJust . snd)
                             $ map (\c -> (c, #{moduleName}.decode c))
                             $ concatMap cases [(0::Integer) ..]
        cases 0 = [[]]
        cases l = concat [[#{moduleName}.O : bs, #{moduleName}.I : bs] | bs <- cases (l-1)]

test1 :: [[Bool]] -> Bool
test1 v = #{moduleName}.decode (#{moduleName}.encode v) == Just v

test2 :: ([#{moduleName}.Bit], Maybe [[Bool]]) -> Bool
test2 (c, mv) = #{moduleName}.encode (fromJust mv) == c
  -- mv = #{moduleName}.decode c, and isJust mv is known
