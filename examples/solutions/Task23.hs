module #{moduleName} where
import Test.HUnit
import Data.List (nub)

{- Read up at #{hoogle} about the type constructor
 - Either.
 -
 - Then give concrete, different, finite values of the following
 - types. Use every data constructor of Maybe and Either at least
 - once.
 -}

value1 :: Either Bool [(Integer,Bool)]
value1 = Right []

value2 :: Either Bool [(Integer,Bool)]
value2 = Left True

value3 :: (Maybe (Either (Integer, Maybe Integer) Bool), Maybe Integer)
value3 = (Nothing, Nothing)

value4 :: (Maybe (Either (Integer, Maybe Integer) Bool), Maybe Integer)
value4 = (Nothing, Just 0)

value5 :: (Maybe (Either (Integer, Maybe Integer) Bool), Maybe Integer)
value5 = (Just (Left (3,Nothing)),Just 1)

value6 :: Either (Maybe Integer, Bool) (Maybe (Integer, Maybe Bool))
value6 = Right Nothing

value7 :: Either (Maybe Integer, Bool) (Maybe (Integer, Maybe Bool))
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
