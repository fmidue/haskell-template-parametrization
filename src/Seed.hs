module Seed (intListToSeed, stringToSeed, seedToString, Seed(..)) where
import Data.Char (ord, digitToInt, isDigit)

newtype Seed = Seed [Int] deriving (Show, Eq)

intListToSeed :: [Int] -> Seed
intListToSeed [] = Seed $ replicate 8 0
intListToSeed seed | l == 8    = Seed $ map (`mod` 10) seed
                   | l < 8     = intListToSeed (seed ++ take (8-l) seed)
                   | otherwise = let (xs, ys) = splitAt 8 seed in intListToSeed $ zipWith (\x y -> mod (x + y) 10) xs (ys ++ take (8 - mod (length ys) 8 + 1) xs)
                   where l     = length seed

stringToSeed :: String -> Seed
stringToSeed str = intListToSeed $ map charToDigit str

seedToString :: Seed -> String
seedToString (Seed []) = ""
seedToString (Seed (x:xs)) = show x ++ seedToString (Seed xs)

charToDigit :: Char -> Int
charToDigit c = if isDigit c then digitToInt c else mod (ord c) 10
