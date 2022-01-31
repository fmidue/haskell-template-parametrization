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
                
--------------------
module Test (test) where
import qualified Main
import Test.QuickCheck
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), (@=?), Test)
import Data.Char

test :: [[ Test ]]
test =
  [[ " Testing 'Aibohphobia'"
    ~: True @=? Main.isPalindrome "Aibohphobia",
    " Testing 'Zerimar Ramirez'"
    ~: True @=? Main.isPalindrome "Zerimar Ramirez",
    " Testing 'racecar'"
    ~: True @=? Main.isPalindrome "racecar",
    " Testing 'bus'"
    ~: False @=? Main.isPalindrome "bus",
    " Testing 'top spot' (spaces are relevant!)"
    ~: False @=? Main.isPalindrome "top spot",
    " Testing with actual palindromes (result should be True):"
    ~: qcWithTimeout 5000 $ forAll validInputs $ \list -> Main.isPalindrome list,
    " Testing with non-palindromes (result should be False):"
    ~: qcWithTimeout 5000 $ forAll invalidInputs $ \list -> not (Main.isPalindrome list)
  ]]

validInputs :: Gen String
validInputs = do s <- listOf (elements "123abc")
                 b <- arbitrary
                 let p = (if b || null s
                          then s ++ reverse s
                          else s ++ tail (reverse s))
                 toUp <- vectorOf (length p) arbitrary
                 return (zipWith (\f c -> if f then toUpper c else c) toUp p)

invalidInputs :: Gen String
invalidInputs = do n <- growingElements [2..100]
                   s <- vectorOf n (elements "ABC123abc")
                   let s' = map toUpper s
                   if s' == reverse s'
                     then invalidInputs
                     else return s