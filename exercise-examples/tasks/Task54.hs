enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
slide = return "151"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
- name-shadowing

#{commonConfigHlintErrors}
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
- Use odd
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Use ++
- Use 1
- "Use :"
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
# - Use isJust
# - Use isNothing
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
# - Use null
- Use or
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Prelude hiding ((!!))
import Test.QuickCheck
import Data.Maybe -- contains useful functions like isNothing, isJust,
                  -- and fromJust; but do also consider slide #{slide}

{- Consider the following two algebraic data types
 - (but ignore the 'deriving' stuff):
 -}

data Tree = Leaf Bool | Node Tree Tree
  deriving (Eq, Show)

data Bit = O | I
  deriving (Eq, Show, Enum, Bounded)

{- Assume we want to encode and decode values of type Tree to and from
 - bit sequences. Essentially, we want to implement serialisation and
 - deserialisation functionality.
 -
 - It so happens that encoding is already solved for us:
 -}

encode :: Tree -> [Bit]
encode (Leaf b)   = [O, bool2bit b]
encode (Node l r) = I : encode l ++ encode r

bool2bit :: Bool -> Bit
bool2bit False = O
bool2bit True  = I

{- So, write another function: -}

decode :: [Bit] -> Maybe Tree
decode = undefined

{- such that:
 -
 - a) for every list of type [Bit] that can be produced by 'encode',
 -    the function 'decode' returns the value 'Just t', where t is
 -    exactly the original tree before applying 'encode', and
 -
 - b) for every list of type [Bit] for which no corresponding original
 -    value of type Tree exists, the function 'decode' returns the
 -    value 'Nothing'.
 -
 - In other words, the general properties corresponding to the
 - following two tests should hold:
 -}

main :: IO ()
main = do quickCheck $ \v -> decode (encode v) == Just v
          quickCheck $ \c -> let mv = decode c
                             in isJust mv ==> encode (fromJust mv) == c

{- If you run this locally, you might get a "Gave up!" message from
 - QuickCheck after fewer than 100 tests for the second property. Do
 - not worry about this. It does not mean that your solution is wrong,
 - only that QuickCheck was not patient enough to search for at least
 - 100 values c with 'decode c' being of the form 'Just ...' as
 - opposed to 'Nothing'. The tests within Autotool are organized a bit
 - differently, so no "giving up" should happen there. Of course, you
 - *should* worry if your local testing produces counterexamples.
 -}

{- HINT:
 -
 - The best strategy to solve this task is to implement a helper
 - function
 -
 -   decode' :: [Bit] -> Maybe (Tree, [Bit])
 -
 - which only decodes a prefix (as long as possible) of the input
 - list, and returns the unused rest unchanged.
 -
 - So the idea is that if
 -
 -   decode' c == Just (v, c')
 -
 - then
 -
 -   encode v ++ c' == c
 -
 - (and decode is defined using decode').
 -
 - For example, decode' [I,O,O,O,I,O,I] then produces the value:
 -
 -   Just (Node (Leaf False) (Leaf True), [O,I])
 -
 - since it holds:
 -
 -   encode (Node (Leaf False) (Leaf True)) == [I,O,O,O,I]
 -
 - And decode' [I,O,I,O,O] produces the value:
 -
 -   Just (Node (Leaf True) (Leaf False), [])
 -
 - "without rest", and hence decode [I,O,I,O,O] simply produces the
 - value:
 -
 -   Just (Node (Leaf True) (Leaf False))
 -}

{- The following definitions are only needed to help QuickCheck.
 - You can ignore them.
 -}

instance Arbitrary Tree where
   arbitrary = sized treegen
     where treegen n | n <= 1 = fmap Leaf arbitrary
           treegen n          = do i <- elements [0 .. n-1]
                                   l <- treegen i
                                   r <- treegen (n-1-i)
                                   return (Node l r)

instance Arbitrary Bit where
  arbitrary = elements [minBound .. maxBound]
----------
module Test (test) where
import qualified #{moduleName}
import Test.QuickCheck
import Test.HUnit ((~:), Test, Assertion, assertFailure, assertBool)
import Data.Maybe

import TestHelper (qcWithTimeoutAndRuns)

test :: [ Test ]
test =
  [ " decoding after encoding" ~: qcWithTimeoutAndRuns 500000 250 test1
  , " encoding after decoding" ~:
    sequence_ [ assertBool (" ... on " ++ show c) (test2 (c, d)) | (c, d) <- instances ] ]
  where instances = take 15 $ filter (isJust . snd)
                            $ map (\c -> (c, #{moduleName}.decode c))
                            $ concatMap cases [(0::Integer) ..]
        cases 0 = [[]]
        cases l = concat [[#{moduleName}.O : bs, #{moduleName}.I : bs] | bs <- cases (l-1)]

test1 :: #{moduleName}.Tree -> Bool
test1 v = #{moduleName}.decode (#{moduleName}.encode v) == Just v

test2 :: ([#{moduleName}.Bit], Maybe #{moduleName}.Tree) -> Bool
test2 (c, mv) = #{moduleName}.encode (fromJust mv) == c
  -- mv = #{moduleName}.decode c, and isJust mv is known
