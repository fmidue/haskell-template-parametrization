module Main where
import Prelude hiding ((!!))
import Test.QuickCheck
import Data.Maybe -- contains useful functions like isNothing, isJust,
                  -- and fromJust; but do also consider slide 160

{- Consider the following enumeration type
 - (but ignore the 'deriving' stuff):
 -}

data Bit = O | I
  deriving (Eq, Show, Enum, Bounded)

{- Assume we want to encode and decode values of type [Either Bool Bool]
 - to and from bit sequences. Essentially, we want to implement
 - serialisation and deserialisation functionality.
 -
 - So, write an *injective* function:
 -}

encodeElement :: Either Bool Bool -> [Bit]
encodeElement (Left True)    = [I, I]
encodeElement (Left False)   = [I, O]
encodeElement (Right True)   = [O, I]
encodeElement (Right False)  = [O, O]

encode :: [Either Bool Bool] -> [Bit]
encode = concatMap encodeElement

{- and another function: -}

decodeElement :: Bit -> Bit -> Either Bool Bool
decodeElement I  I  = Left True
decodeElement I  O  = Left False
decodeElement O  I  = Right True
decodeElement O  O  = Right False

decode' :: [Bit] -> [Either Bool Bool]
decode' []        = []
decode' [_]       = []
decode' (a:b:xs)  = decodeElement a b : decode' xs

decode :: [Bit] -> Maybe [Either Bool Bool]
decode xs =  if even (length xs) then Just (decode' xs) else Nothing

{- such that:
 -
 - a) for every list of type [Bit] that can be produced by 'encode',
 -    the function 'decode' returns the value 'Just l', where l is
 -    exactly the original list before applying 'encode', and
 -
 - b) for every list of type [Bit] for which no corresponding original
 -    list of type [Either Bool Bool] exists, the function 'decode'
 -    returns the value 'Nothing'.
 -
 - In other words, the general properties corresponding to the
 - following two tests should hold:
 -}

main :: IO ()
main = do quickCheck $ \v -> decode (encode v) == Just v
          quickCheck $ \c -> let mv = decode c
                             in isJust mv ==> encode (fromJust mv) == c

{- Also, for the purposes of serialisation, it is wise to use a
 - space efficient encoding, so try to not create unnecessarily
 - long Bit-lists for given (Either Bool Bool)-lists in general.
 -}

{- The following definition is only needed to help QuickCheck.
 - You can ignore it.
 -}

instance Arbitrary Bit where
  arbitrary = elements [minBound .. maxBound]
