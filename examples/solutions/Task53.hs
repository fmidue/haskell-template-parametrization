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

{- Assume we want to encode and decode values of type [[Bool]] to and
 - from bit sequences. Essentially, we want to implement serialisation
 - and deserialisation functionality.
 -
 - So, write an *injective* function:
 -}

encode :: [[Bool]] -> [Bit]
encode []            = []
encode ([]:bss)      = O : encode bss
encode ((b:bs):bss)  = I : bool2bit b : encode (bs:bss)

bool2bit :: Bool -> Bit
bool2bit False  = O
bool2bit True   = I

{- and another function: -}
bit2bool :: Bit -> Bool
bit2bool O  = False
bit2bool I  = True

decode :: [Bit] -> Maybe [[Bool]]
decode []        = Just []
decode (O:xs)    =  case decode xs of
                      Just bss -> Just ([]:bss)
                      Nothing -> Nothing
decode (I:x:xs)  =  case decode xs of
                      Just (bs:bss) -> Just ((bit2bool x : bs) : bss)
                      _ -> Nothing
decode _         = Nothing

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
