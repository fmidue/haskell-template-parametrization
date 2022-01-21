dat = return "[(\"T\",\"A False | B [2,5,7]\"),(\"U\",\"C True V | D V\"),(\"V\",\"E 9 | F (5, True)\"),(\"W\",\"G V | H | I U\")]"
value1:plain_dataBuilder = return $ generateData "T" 0 0 False #{seed} #{dat}
value2 {

#{plain_dataBuilder}

value2 :: IO String
value2 = return $ snd (getDifferentData "#{value1}" #{seed})

getDifferentData :: String -> Int -> (Int, String)
getDifferentData str seed = let (a, b) = getDifferentData str (seed + 1) in if str == dat then getDifferentData str (seed + 1) else (seed, dat)
    where dat = generateData "T" 0 0 False seed #{dat}

}
value3:plain_dataBuilder = return $ generateData "U" 1 1 False #{seed} #{dat}
value4:plain_dataBuilder = return $ generateData "(U, V)" 1 2 False #{seed} #{dat}
value5:plain_dataBuilder = return $ generateData "(V, W)" 1 2 False #{seed} #{dat}
value6:plain_dataBuilder = return $ generateData "(W, U)" 1 3 False #{seed} #{dat}
value7:plain_dataBuilder = return $ generateData "W" 1 2 False #{seed} #{dat}
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
value1 = #{value1}

value2 :: T
value2 = #{value2}

value3 :: U
value3 = #{value3}

value4 :: (U, V)
value4 = #{value4}

value5 :: (V, W)
value5 = #{value5}

value6 :: (W, U)
value6 = #{value6}

value7 :: W
value7 = #{value7}

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
