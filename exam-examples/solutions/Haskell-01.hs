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
encode 0 = [O]
encode x = sign x : reverse (toBin (abs x))
  where
    sign x = if x > 0 then I else O
    toBin :: Integer -> [Bit]
    toBin i =
      case quotRem i 2 of
        (0,0) -> [O]
        (0,1) -> [I]
        (j,0) -> O : toBin j
        (j,_) -> I : toBin j

-- and another function:

decode :: [Bit] -> Maybe Integer
decode [] = Nothing
decode [O] = Just 0
decode (s:xs) = case toDec xs of
  Just n
    | n > 0 && s == I -> Just n
    | n > 0 && s == O -> Just $ negate n
    | otherwise  -> Nothing
  Nothing -> Nothing

toDec :: [Bit] -> Maybe Integer
toDec [] = Nothing
toDec (O:_) = Nothing
toDec xs = Just $ sum $ zipWith (\b x -> if b == I then x else 0) xs [ 2^i | i <- [length xs-1, length xs-2 .. 0]]

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
