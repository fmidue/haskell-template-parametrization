rdmSelection = withCurrentSeed (shuffle [1,0,1,0,1])
bug1 = return $ ["string", "String"]!!(#{rdmSelection}!!0)
bug2 = return $ ["Reverse", "reverse"]!!(#{rdmSelection}!!1)
bug3 = return $ ["let", "where"]!!(#{rdmSelection}!!2)
bug4 = return $ ["isPalindrom", "isPalindrome"]!!(#{rdmSelection}!!3)
bug5 = return $ ["low", "list"]!!(#{rdmSelection}!!4)
--------------------
module Main where
import Data.Char (toUpper, toLower)
import Test.QuickCheck

isPalindrome :: #{bug1} -> Bool
isPalindrome list = #{bug5} == #{bug2} low
  #{bug3} low = [ toLower c | c <- list ]

main :: IO ()
main = do putStrLn "A single character is a palindrome:"
          quickCheck (\c -> isPalindrome [c])
          putStrLn "Adding the same character to the front and back of a string does not change the outcome of isPalindrome:"
          quickCheck (\c str -> isPalindrome ([toUpper c] ++ str ++ [c]) == #{bug4} str) 
