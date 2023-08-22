module #{moduleName} where
import Prelude hiding (($), (!!), take, drop, splitAt)
import Test.QuickCheck

{-
Write a function 'hasEvenEvens' for the given type signature which returns
whether a list contains an even amount of even numbers.

By executing 'main' below, you can test your solution
before uploading it.
-}

hasEvenEvens :: [Integer] -> Bool
hasEvenEvens xs = even (length [ x | x <- xs, even x ])

-- needs more than one equation
hasEvenEvens' :: [Integer] -> Bool
hasEvenEvens' xs
  | null xs = True
  | isSingleton = not evenHead
  | evenHead = not recursiveEven
  | otherwise = recursiveEven
  where
    isSingleton = length xs == 1
    evenHead = even (head xs)
    recursiveEven = hasEvenEvens' (tail xs)

hasEvenEvens'' :: [Integer] -> Bool
hasEvenEvens'' xs =
  let
    isSingleton = length xs == 1
    evenHead = even (head xs)
    recursiveEven = hasEvenEvens'' (tail xs)
  in null xs || (isSingleton && not evenHead) || (evenHead && not recursiveEven) || (recursiveEven && not evenHead)

hasEvenEvens''' :: [Integer] -> Bool
hasEvenEvens''' xs = null xs || (evenHead /= recursiveEven)
  where
    evenHead = even (head xs)
    recursiveEven = hasEvenEvens''' (tail xs)

main :: IO ()
main = do putStrLn "Empty list [] has an even amount of even numbers:"
          quickCheck (hasEvenEvens [])
          putStrLn "Singleton list [x] cannot have an even amount of even numbers if x is even:"
          quickCheck (\x -> even x ==> not (hasEvenEvens [x]))
          putStrLn "Singleton list [x] has an even amount of even numbers if x is odd:"
          quickCheck (\x -> odd x ==> hasEvenEvens [x])
          putStrLn "A combined list contains an even amount of even numbers exactly if the separate lists both have even or both have odd many even numbers:"
          quickCheck (\xs ys -> hasEvenEvens (xs ++ ys) == (hasEvenEvens xs == hasEvenEvens ys))
