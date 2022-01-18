module Main where
import Test.HUnit
import Data.List (nub)

{- Read up on https://hoogle.haskell.org about the type constructor
 - Either.
 -
 - Then give concrete, different, finite values of the following
 - types. Use every data constructor of Maybe and Either at least
 - once.
 -}

value1 :: #{gen_value1}
value1 = Right []

value2 :: #{gen_value2}
value2 = Left True

value3 :: #{gen_value3}
value3 = (Nothing, Nothing)

value4 :: #{gen_value4}
value4 = (Nothing, Just 0)

value5 :: #{gen_value5}
value5 = (Just (Left (3,Nothing)),Just 1)

value6 :: #{gen_value6}
value6 = Right Nothing

value7 :: #{gen_value7}
value7 = Left (Nothing, False)

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "value1 and value2 are different" ~:
            (value1 /= value2) @?= True
          _ <- runTestTT $ "value3, value4, value5 are pairwise different" ~:
            (nub [value3, value4, value5] == [value3, value4, value5]) @?= True
          _ <- runTestTT $ "value6 and value7 are different" ~:
            (value6 /= value7) @?= True
          return ()
