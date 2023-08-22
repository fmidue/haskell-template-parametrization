enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
otherTask = return "Task54"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- missing-signatures
- name-shadowing
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use ++
- Use 1
- "Use :"
- Use all
- Use and
- Use any
- Use camelCase
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use even
- Use find
- Use floor
- Use foldr
- Use fromMaybe
- Use guards
- Use if
- Use infix
- Use lefts
- Use list comprehension
- Use map
- Use map once
- Use mapMaybe
- Use maximum
# - Use maybe
- Use minimum
- Use negate
- Use newtype instead of data
- Use notElem
- Use odd
- Use or
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tuple-section
# - Use uncurry
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Use foldl
- Use tail

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Prelude hiding ((!!))
import Data.List
import Data.Maybe
import Test.QuickCheck

{- Consider the following known data types: -}

data Bit    = O | I                              deriving (Read, Show, Eq)
data Tree a = Leaf a | Node (Tree a) a (Tree a)  deriving (Show, Eq)

{- Your task is to write a function which, for values of type
 - Tree Bit, reverses the effect of 'show', meaning that it takes a
 - string representation and converts it into an actual value of type
 - Tree Bit.
 -}

parse :: String -> Tree Bit
parse = undefined

{- For this you may assume that only valid string representations will
 - be given to you, so you don't have to add any error handling and
 - your function only has to handle the proper conversion, in the
 - sense that 'parse (show t)' returns t, for all t :: Tree Bit.
 -}

test :: Property
test = forAll (elements [1..6]) $ \(Blind h) -> forAll (sizedTree h)
                                $ \t -> parse (show t) == t

main :: IO ()
main = quickCheck test

{- The show function, which has to be "reversed" here, is *not* any
 - manually written function, but a function automatically generated
 - by Haskell (see "deriving (Show, Eq)" at the top).
 -
 - Your function has to, for example, satisfy the following:
 -
 -      parse "Node (Node (Leaf I) O (Leaf I)) I (Leaf O)"
 -   == Node (Node (Leaf I) O (Leaf I)) I (Leaf O)
 -
 - Hint for possible solutions:
 -
 - It's worthwhile to consider using an idea from #{otherTask},
 - specifically the generalization of decode to decode'. Try to
 - implement parse via a function parse' :: String -> (Tree Bit, String),
 - for which parse' (show t ++ s) == (t,s) should always hold.
 -}

{- The following are just helper definitions for QuickCheck: -}

instance Arbitrary Bit where
  arbitrary = elements [O,I]

sizedTree :: Arbitrary a => Integer -> Gen (Tree a)
sizedTree 0 = fmap Leaf arbitrary
sizedTree n = frequency [ (1, sizedTree 0), (2^n, branching) ]
  where branching = do t1 <- sizedTree (n-1)
                       a  <- arbitrary
                       t2 <- sizedTree (n-1)
                       return (Node t1 a t2)
-------------------------------------
module Test (test) where
import TestHelper (qcWithTimeout)
import qualified #{moduleName}

test :: [IO ()]
test =
  [qcWithTimeout 500000 #{moduleName}.test]
