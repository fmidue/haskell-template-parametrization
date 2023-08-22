module Main where
import Test.HUnit
import Data.List (nub)

-- Give concrete, different, finite values of the following types.
-- Use each data constructor of #{wordingWatermark} at least once.

value1 :: Either Bool (#{pairTypes})
value1 = Left True

value2 :: Either Bool (#{pairTypes})
value2 = Right (#{pairValues})

value3 :: (Either (Maybe Bool) (Integer, Maybe Integer), Maybe Integer)
value3 = (Left Nothing, Nothing)

value4 :: Maybe (Either Integer (Maybe Bool, Integer, Maybe Bool))
value4 = Just (Left 0)

value5 :: Maybe (Either Integer (Maybe Bool, Integer, Maybe Bool))
value5 = Nothing

value6 :: Maybe (Either Integer (Maybe Bool, Integer, Maybe Bool))
value6 = Just (Right (Nothing, 1, Just True))

-- A very simple test suite:

main :: IO ()
main = do _ <- runTestTT $ "not value1, value2 equal" ~:
            (value1 == value2) @?= False
          _ <- runTestTT $ "value4, value5, value6 pairwise different" ~:
            (nub [value4, value5, value6] == [value4, value5, value6]) @?= True
          return ()
