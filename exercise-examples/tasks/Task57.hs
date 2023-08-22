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
import Prelude hiding (replicate)

{- A Haskell function 'replicate' should generate a list containing
 - the same element (of any given type) n times (with n >= 0). Given
 - is the following type signature:
 -}

replicate :: Int -> a -> [a]

{- where the integer argument is n (the amount of entries in the
 - list).
 -
 - Write the complete function definition without using helper
 - functions:
 -}

replicate = undefined
-------------------------------------
module Test (test) where
import Prelude
import qualified #{moduleName}
import TestHelper (qcWithTimeout)
import Test.HUnit ((~:), Test)
import Test.QuickCheck

test :: [ Test ]
test = [ " test with random inputs" ~:
            qcWithTimeout 500000 $ forAll (growingElements [0..1000]) $ \n -> #{moduleName}.replicate n 'a' == replicate n 'a'
       ]
