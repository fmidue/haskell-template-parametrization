{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Test.HUnit
import Data.List (nub)

data T = A Bool | B [Int]
data U = C Bool V | D V
data V = E Int | F (Int, Bool)
data W = G V | H | I U

-- Give concrete, different, finite values of the following types.
-- Use every data constructor of the types defined above at least
-- once. Also, do not reuse any subexpressions.

value1 :: T
value1 = A True

value2 :: T
value2 = B [7,13]

value3 :: U
value3 = C False (E 11)

value4 :: (U, V)
value4 = (D (E 2), F (1,False))

value5 :: (V, W)
value5 = (E 2, G (E 4))

value6 :: (W, U)
value6 = (I (D (F (8,True))), C True (E 0))

value7 :: W
value7 = H

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "value1 and value2 are different" ~:
            (value1 /= value2) @?= True
          _ <- runTestTT $ "value3, fst value4, snd value6 are pairwise different" ~:
            (nub [value3, fst value4, snd value6] == [value3, fst value4, snd value6]) @?= True
          _ <- runTestTT $ "snd value4 and fst value5 are different" ~:
            (snd value4 /= fst value5) @?= True
          _ <- runTestTT $ "snd value5, fst value6, value7 are pairwise different" ~:
            (nub [snd value5, fst value6, value7] == [snd value5, fst value6, value7]) @?= True
          return ()

deriving instance Eq T
deriving instance Eq U
deriving instance Eq V
deriving instance Eq W
