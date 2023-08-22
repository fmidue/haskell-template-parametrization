enableWhitespaceWatermarking = return "True"
moduleName = return "Task10"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
# - incomplete-patterns # might reveal list patterns
# - incomplete-uni-patterns # might reveal list patterns
- missing-signatures
- name-shadowing
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- unused-local-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Use ++
- Use 1
- Use all
- Use and
- Use any
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use find
- Use floor
- Use foldl
- Use foldr
- Use fromMaybe
- Use infix
- Use lefts
- Use list comprehension
- Use map once
- Use mapMaybe
# - Use maybe
- Use negate
- Use newtype instead of data
- Use notElem
- Use or
- Use repeat
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
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
isPalindrome = undefined

main :: IO ()
main = do putStrLn "A single character is a palindrome:"
          quickCheck (\c -> isPalindrome [c])
          putStrLn "Adding the same character to the front and back of a string does not change the outcome of isPalindrome:"
          quickCheck (\c str -> isPalindrome ([toUpper c] ++ str ++ [c]) == isPalindrome str)
----------
module Test (test) where
import qualified #{moduleName}
import Test.QuickCheck
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), (@=?), Test)
import Data.Char

test :: [[ Test ]]
test =
  [[ " Testing 'Aibohphobia'"
    ~: True @=? #{moduleName}.isPalindrome "Aibohphobia",
    " Testing 'Zerimar Ramirez'"
    ~: True @=? #{moduleName}.isPalindrome "Zerimar Ramirez",
    " Testing 'racecar'"
    ~: True @=? #{moduleName}.isPalindrome "racecar",
    " Testing 'bus'"
    ~: False @=? #{moduleName}.isPalindrome "bus",
    " Testing 'top spot' (spaces are relevant!)"
    ~: False @=? #{moduleName}.isPalindrome "top spot",
    " Testing with actual palindromes (result should be True):"
    ~: qcWithTimeout 5000 $ forAll validInputs $ \list -> #{moduleName}.isPalindrome list,
    " Testing with non-palindromes (result should be False):"
    ~: qcWithTimeout 5000 $ forAll invalidInputs $ \list -> not (#{moduleName}.isPalindrome list)
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
