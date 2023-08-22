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

{- Define an appropriate function 'mult' for the data type definition
 - below:
 -}

data Nat = Zero | Succ Nat  deriving Show

mult :: Nat -> Nat -> Nat
mult = undefined
-------------------------------------
{-# LANGUAGE StandaloneDeriving #-}
module Test (test) where
import Prelude
import #{moduleName} (Nat(..))
import qualified #{moduleName}
import TestHelper (qcWithTimeoutAndRuns)
import Test.QuickCheck

mult :: Nat -> Nat -> Nat
mult Zero _         = Zero -- jv: macht nur bei unendlichen Eingaben einen Unterschied.
mult _    Zero      = Zero
mult n1   (Succ n2) = n1 `add` mult n1 n2

add :: Nat -> Nat -> Nat
add Zero      n  = n
add (Succ n1) n2 = Succ (add n1 n2)

test :: [IO ()]
test =
  [qcWithTimeoutAndRuns 5000000 1000 $ \n1 n2 -> #{moduleName}.mult n1 n2 == mult n1 n2]

deriving instance Eq Nat

instance Arbitrary Nat where
   arbitrary = sized $ \n -> growingElements $ scanl (const . Succ) Zero [1..n]
