module Main where
import Prelude hiding (($), (!!), map, foldr)
import Data.Char (toUpper, toLower)
import Test.QuickCheck

-- Write a function for the given 'isPalindrome' type signature which
-- returns whether the given string is a palindrome.
--
-- A palindrome is a word or collection of words that is the same read
-- forward and backward, such as "racecar", "Zerimar Ramirez", or
-- "Aibohphobia" (the medical term for irrational fear of
-- palindromes).
--
-- Note that our concept of palindromes here ignores whether a letter
-- appears in lower case or upper case.
--
-- By executing 'main' below, you can partially test your solution
-- before uploading it.

isPalindrome :: String -> Bool
isPalindrome list = low == reverse low
  where low = [ toLower c | c <- list ]

isPalindrome' :: String -> Bool
isPalindrome' s | length s < 2 = True
isPalindrome' s = toLower (head s) == toLower (last s) && isPalindrome' (init (tail s))

isPalindrome'' :: String -> Bool
isPalindrome'' xs = and [ toUpper x == toUpper y | (x,y) <- zip xs ys ]
  where ys = reverse xs

main :: IO ()
main = do putStrLn "A single character is a palindrome:"
          quickCheck (\c -> isPalindrome [c])
          putStrLn "Adding the same character to the front and back of a string does not change the outcome of isPalindrome:"
          quickCheck (\c str -> isPalindrome ([toUpper c] ++ str ++ [c]) == isPalindrome str)
