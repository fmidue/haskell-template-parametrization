dat = return "[(\"T\",\"A False | B [2,5,7]\"),(\"U\",\"C True V | D V\"),(\"V\",\"E 9 | F (5, True)\"),(\"W\",\"G V | H | I U\")]"
plain_data {
data T = #{t} deriving (Show, Read, Eq)
data U = #{u} deriving (Show, Read, Eq)
data V = #{v} deriving (Show, Read, Eq)
data W = #{w} deriving (Show, Read, Eq)
}
value1:plain_dataBuilder = return $ generateData "T" 0 0 #{seed} #{dat}
value2 {

#{plain_dataBuilder}

value2 :: IO String
value2 = return $ getDifferentData #{seed}

getDifferentData :: Int -> String
getDifferentData seed = if "#{value1}" == dat then getDifferentData (seed + 1) else dat
    where dat = generateData "T" 0 0 seed #{dat}

}
value3:plain_dataBuilder = return $ generateData "U" 1 1 #{seed} #{dat}
value4 {

#{plain_dataBuilder}

#{plain_data}

value4 :: IO String
value4 = return $ getDifferentData #{seed}

getDifferentData :: Int -> String
getDifferentData seed = if head "#{value3}" == head (show k) then getDifferentData (seed + 1) else show (k, j)
    where (k, j) = read (generateData "(U, V)" 0 1 seed #{dat}) :: (U, V)

}
value5 {

#{plain_dataBuilder}

#{plain_data}

value5 :: IO String
value5 = return $ getDifferentData #{seed}

getDifferentData :: Int -> String
getDifferentData seed = if head (show value4) == head (show k) then getDifferentData (seed + 1) else show (k, j)
    where (k, j) = read (generateData "(V, W)" 0 2 seed #{dat}) :: (V, W)
          value4 = snd (#{value4})

}
value6 {

#{plain_dataBuilder}

#{plain_data}

value6 :: IO String
value6 = return $ getDifferentData #{seed}

getDifferentData :: Int -> String
getDifferentData seed = if head (show value5) == head (show k) || show value3 == show j || show value4 == show j then getDifferentData (seed + 1) else show (k, j)
    where (k, j) = read (generateData "(W, U)" 0 2 seed #{dat}) :: (W, U)
          value5 = snd (#{value5})
          value3 = #{value3}
          value4 = fst (#{value4})

}
value7 {

#{plain_dataBuilder}

#{plain_data}

value7 :: IO String
value7 = return $ getDifferentData #{seed}

getDifferentData :: Int -> String
getDifferentData seed = if head (show value5) == head (show k)  || head (show value6) == head (show k) then getDifferentData (seed + 1) else show k
    where k = read (generateData "W" 0 2 seed #{dat}) :: W
          value5 = snd (#{value5})
          value6 = fst (#{value6})

}
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
