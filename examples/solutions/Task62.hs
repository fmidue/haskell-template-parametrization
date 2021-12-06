module Main where
import Prelude hiding ((!!))
import Data.List
import Data.Maybe
import Test.QuickCheck

{- Consider the following known data types: -}

data Bit    = O | I                              deriving (Read, Show, Eq)
data Tree a = Leaf a | Node (Tree a) a (Tree a)  deriving (Show, Eq)

{- Your task is to write a function which, for values of type
 - Tree Bit, reverses the effect of 'show', meaning that it takes a
 - string representation and converts it into an actual value of type
 - Tree Bit.
 -}

parse :: String -> Tree Bit
parse s | null rest = t
  where (t,rest) = parse' s

parse' :: String -> (Tree Bit, String)
parse' s | cons == "Leaf " = (Leaf (read bit1), rest1)
         | cons == "Node " = (Node t1 (read [bit2]) t2, rest3)
  where (cons, rest) = splitAt 5 s
        (bit1, rest1) = splitAt 1 rest
        (t1, ')':' ':bit2:' ':'(':rest2) = parse' $ case rest of '(':s' -> s'
        (t2, ')':rest3) = parse' rest2

{- For this you may assume that only valid string representations will
 - be given to you, so you don't have to add any error handling and
 - your function only has to handle the proper conversion, in the
 - sense that 'parse (show t)' returns t, for all t :: Tree Bit.
 -}

test :: Property
test = forAll (elements [1..6]) $ \(Blind h) -> forAll (sizedTree h)
                                $ \t -> parse (show t) == t

main :: IO ()
main = quickCheck test

{- The show function, which has to be "reversed" here, is *not* any
 - manually written function, but a function automatically generated
 - by Haskell (see "deriving (Show, Eq)" at the top).
 -
 - Your function has to, for example, satisfy the following:
 -
 -      parse "Node (Node (Leaf I) O (Leaf I)) I (Leaf O)"
 -   == Node (Node (Leaf I) O (Leaf I)) I (Leaf O)
 -
 - Hint for possible solutions:
 -
 - It's worthwhile to consider using an idea from Task 51,
 - specifically the generalization of decode to decode'. Try to
 - implement parse via a function parse' :: String -> (Tree Bit, String),
 - for which parse' (show t ++ s) == (t,s) should always hold.
 -}

{- The following are just helper definitions for QuickCheck: -}

instance Arbitrary Bit where
  arbitrary = elements [O,I]

sizedTree :: Arbitrary a => Integer -> Gen (Tree a)
sizedTree 0 = fmap Leaf arbitrary
sizedTree n = frequency [ (1, sizedTree 0), (2^n, branching) ]
  where branching = do t1 <- sizedTree (n-1)
                       a  <- arbitrary
                       t2 <- sizedTree (n-1)
                       return (Node t1 a t2)
