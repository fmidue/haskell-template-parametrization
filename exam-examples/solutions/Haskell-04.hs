module Main where
import Prelude hiding (elem, notElem)
import Test.QuickCheck

-- Consider the following definitions:

noDuplicates :: [Integer] -> [Integer]
noDuplicates [] = []
noDuplicates (x:xs) | notElem x ys = x:ys
                    | otherwise    = ys
   where ys = noDuplicates xs

notElem :: Integer -> [Integer] -> Bool
notElem _ []             = True
notElem x (y:_) | x == y = False
notElem x (_:ys)         = notElem x ys

-- Write semantically equivalent #{wordingWatermark} of noDuplicates and notElem
-- using foldr.

noDuplicates' :: [Integer] -> [Integer]
noDuplicates' = foldr (\x ys -> if notElem' x ys then x:ys else ys) []

notElem' :: Integer -> [Integer] -> Bool
notElem' x = foldr (\y b -> if x == y then False else b) True

-- By executing 'main' below, you can test your solution before
-- uploading it.

main :: IO ()
main = do quickCheck $ \xs -> noDuplicates xs == noDuplicates' xs
          quickCheck $ \x xs -> notElem x xs == notElem' x xs
