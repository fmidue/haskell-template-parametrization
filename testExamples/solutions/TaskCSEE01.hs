module Main where
import Data.Char (toUpper, toLower)
import Test.QuickCheck

isPalindrome :: String -> Bool
isPalindrome list = #{rev1}low == #{rev2}low
  where low = [ toLower c | c <- list ]

main :: IO ()
main = do putStrLn "A single character is a palindrome:"
          quickCheck (\c -> isPalindrome [c])
          putStrLn "Adding the same character to the front and back of a string does not change the outcome of isPalindrome:"
          quickCheck (\c str -> isPalindrome ([toUpper c] ++ str ++ [c]) == isPalindrome str)