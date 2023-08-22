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
encode = concatMap encodeAnimal

encodeAnimal :: Animal -> [Bit]
encodeAnimal Cat  = [I,I]
encodeAnimal Dog  = [I,O]
encodeAnimal Bird = [O]

{- and another function: -}

decode :: [Bit] -> Maybe [Animal]
decode []  = Just []
decode [I] = Nothing
decode (O:bs) =
  case decode bs of
    Just rest -> Just (Bird : rest)
    Nothing -> Nothing
decode (I:b:xs) =
  case decode xs of
    Just rest -> Just ((if b == I then Cat else Dog) : rest)
    Nothing -> Nothing

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
