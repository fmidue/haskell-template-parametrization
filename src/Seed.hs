{-|
Module      : Seed

Contains functions to translate to or from a seed.
-}

module Seed (
    -- * Type
    Seed(..),
    -- * Functions
    intListToSeed, stringToSeed, seedToString) where
import Data.Char (ord, digitToInt, isDigit)

-- | Seed is a list of ints
newtype Seed = Seed [Int] deriving (Show, Eq)

-- | Translates any int list to a seed with a fixed size of 8
intListToSeed :: [Int] -> Seed
intListToSeed [] = Seed $ replicate 8 0
intListToSeed seed | l == 8    = Seed $ map (`mod` 10) seed
                   | l < 8     = intListToSeed (seed ++ take (8-l) seed)
                   | otherwise = let (xs, ys) = splitAt 8 seed in intListToSeed $ zipWith (\x y -> mod (x + y) 10) xs (ys ++ take (8 - mod (length ys) 8 + 1) xs)
                   where l     = length seed

-- | Translates any string to a seed with a fixed size of 8
stringToSeed :: String -> Seed
stringToSeed str = intListToSeed $ map charToDigit str

-- | Translates seed to string
seedToString :: Seed -> String
seedToString (Seed []) = ""
seedToString (Seed (x:xs)) = show x ++ seedToString (Seed xs)

charToDigit :: Char -> Int
charToDigit c = if isDigit c then digitToInt c else mod (ord c) 10
