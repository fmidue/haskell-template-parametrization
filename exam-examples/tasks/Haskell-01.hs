enableWhitespaceWatermarking = return "True"
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
import Data.Maybe

-- Consider the following enumeration type
-- (but ignore the 'deriving' stuff):

data Bit = O | I
  deriving (Eq, Show, Enum, Bounded)

-- Assume we want to encode and decode values of type Integer
-- (including negative numbers) to and from bit sequences.
-- Essentially, we want to implement serialisation and deserialisation
-- functionality.
--
-- So, write an *injective* function:

encode :: Integer -> [Bit]
encode = undefined

-- and another function:

decode :: [Bit] -> Maybe Integer
decode = undefined

-- such that:
--
-- a) for every list of type [Bit] that can be produced by 'encode',
--    the function 'decode' returns the value 'Just i', where i is
--    exactly the original integer before applying 'encode', and
--
-- b) for every list of type [Bit] for which no corresponding original
--    value of type Integer exists, the function 'decode' returns the
--    value 'Nothing'.
--
-- In other words, the general properties corresponding to the
-- following two tests should hold:

main :: IO ()
main = do quickCheck $ \v -> decode (encode v) == Just v
          quickCheck $ \c -> let mv = decode c
                             in isJust mv ==> encode (fromJust mv) == c

-- Also, for the purposes of serialisation, it is wise to use a space
-- efficient encoding, so do not create excessively long Bit-lists for
-- given Integer-values in general.

-- The following definition is only needed to help QuickCheck.
-- You can ignore it.

instance Arbitrary Bit where
  arbitrary = elements [minBound .. maxBound]
----------
module Test (test) where
import qualified Main
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
    [ " one Bit for 0?" ~:
      length (Main.encode 0) == 1 @?= True
    , " less than n Bits to encode n (for 'non-small' n)?" ~:
      qcWithTimeoutAndRuns 5000 100 $ \i -> (abs i) > 4 ==> (fromIntegral (length (Main.encode i)) < abs i)
    ]
  ]
  where instances = take 1000 $ filter (isJust . snd)
                              $ map (\c -> (c, Main.decode c))
                              $ concatMap cases [(0::Integer) ..]
        cases 0 = [[]]
        cases l = concat [[Main.O : bs, Main.I : bs] | bs <- cases (l-1)]

test1 :: Integer -> Bool
test1 v = Main.decode (Main.encode v) == Just v

test2 :: ([Main.Bit], Maybe Integer) -> Bool
test2 (c, mv) = Main.encode (fromJust mv) == c
  -- mv = Main.decode c, and isJust mv is known
