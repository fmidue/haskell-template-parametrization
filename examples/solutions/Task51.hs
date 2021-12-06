module Main where
import Test.HUnit
import Data.List (nub)

{- Give concrete, different, finite values of the following types.
 - Use every data constructor of Maybe and Either at least once.
 -}

value1 :: Either Bool [Bool]
value1 = Right []

value2 :: Either Bool [Bool]
value2 = Left True

value3 :: Either Bool [Bool]
value3 = Right [True]

value4 :: (Either (Integer, Maybe Integer) (Maybe Bool), Maybe Integer)
value4 = (Left (3, Nothing), Just 1)

value5 :: Maybe (Either (Maybe Integer, Bool) (Integer, Maybe Bool))
value5 = Nothing

value6 :: Maybe (Either (Maybe Integer, Bool) (Integer, Maybe Bool))
value6 = Just (Left (Nothing, False))

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "value1, value2, value3 pairwise different" ~:
            (nub [value1, value2, value3] == [value1, value2, value3]) @?= True
          _ <- runTestTT $ "value5, value6 different" ~:
            (value5 /= value6) @?= True
          return ()
