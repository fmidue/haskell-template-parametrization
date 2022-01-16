dat = return "[(\"T\",\"A Bool | B [Int]\"),(\"U\",\"C Bool V | D V\"),(\"V\",\"E Int | F (Int, Bool)\"),(\"W\",\"G V | H | I U\")]"
gen_value1 = "T" 0 False [1,2,4,6,4,6,4] (M.fromList #{dat})
gen_value2 = "T" 0 False [1,2,4,6,4,6,4] (M.fromList #{dat})
gen_value3 = "U" 1 False [1,2,4,6,4,6,4] (M.fromList #{dat})
gen_value4 = "(U, V)" 2 False [1,2,4,6,4,6,4] (M.fromList #{dat})
gen_value5 = "(V, W)" 2 False [1,2,4,6,4,6,4] (M.fromList #{dat})
gen_value6 = "(W, U)" 2 False [1,2,4,6,4,6,4] (M.fromList #{dat})
gen_value7 = "W" 2 False [1,2,4,6,4,6,4] (M.fromList #{dat})
----
{-# LANGUAGE StandaloneDeriving #-}
module Main where
import Test.HUnit
import Data.List (nub)

data T = #{t}
data U = #{u}
data V = #{v}
data W = #{w}

-- Give concrete, different, finite values of the following types.
-- Use every data constructor of the types defined above at least
-- once. Also, do not reuse any subexpressions.

value1 :: T
value1 = #{gen_value1}

value2 :: T
value2 = #{gen_value2}

value3 :: U
value3 = #{gen_value3}

value4 :: (U, V)
value4 = #{gen_value4}

value5 :: (V, W)
value5 = #{gen_value5}

value6 :: (W, U)
value6 = #{gen_value6}

value7 :: W
value7 = #{gen_value7}

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
