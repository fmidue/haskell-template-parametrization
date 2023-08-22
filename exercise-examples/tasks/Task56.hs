enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
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

configGhcWarnings: []

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Use foldl
- Use tail

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Prelude hiding ((!!), head, take, drop, length)

{- Implement a function 'dropEven' in Haskell which, given a list,
 - removes every second element:
 -}

dropEven :: [a] -> [a]
dropEven = undefined
-------------------------------------
module Test (test) where
import Prelude
import #{moduleName} (dropEven)
import Test.QuickCheck
import Test.HUnit ((~:), Test, Assertion, assertFailure)

import TestHelper (qcWithTimeoutAndRuns)

limit :: Int
limit = 9273

test :: [IO ()]
test =
   [qcWithTimeoutAndRuns 5000000 100 $ \(N n) ->
               let odds = dropEven [1..n]
               in all odd odds && length odds == n `div` 2 + if odd n then 1 else 0]

newtype N = N Int
instance Arbitrary N where
   arbitrary = do
      n <- growingElements [0..limit]
      return (N n)
   shrink (N n) = [N i | i <- [0..n-1]]
instance Show N where
   show (N n) = show [1..n]
