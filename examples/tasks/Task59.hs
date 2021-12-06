configGhcErrors:
- deprecation
- empty-enumerations
- identities
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
- unused-local-binds
- unused-matches
- unused-pattern-binds
configHlintErrors:
- Apply De Morgan law
- Avoid lambda
- Avoid reverse
- Collapse lambdas
- Eta reduce
- Evaluate
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant if
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use &&
- Use ++
- Use /=
- Use 1
- "Use :"
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use all
- Use and
- Use any
- Use camelCase
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use drop
- Use elem
- Use even
- Use find
- Use floor
- Use foldr
- Use fromMaybe
- Use fst
- Use guards
- Use head
- Use id
- Use if
- Use infix
- Use init
- Use last
- Use left fold instead of right fold
- Use lefts
- Use list comprehension
- Use list literal pattern
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
- Use otherwise
- Use product
- Use repeat
- Use replicate
- Use right fold instead of left fold
- Use rights
- Use snd
- Use splitAt
- Use sqrt
- Use sum
- Use take
- Use tuple-section
# - Use uncurry
- Use ||
- Used otherwise as a pattern
- Using all on tuple
- Using and on tuple
- Using any on tuple
- Using concat on tuple
- Using elem on tuple
- Using foldr on tuple
- Using length on tuple
- Using maximum on tuple
- Using minimum on tuple
- Using null on tuple
- Using or on tuple
- Using product on tuple
- Using sum on tuple
allowAdding: true
allowModifying: false
allowRemoving: false
configHlintGroups:
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings: []
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Avoid lambda using `infix`
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
- Use foldl
- Use section
- Use tail
configLanguageExtensions:
- NoTemplateHaskell
- TupleSections
# configLanguageExtensions - this sets LanguageExtensions for hlint as well
# configHlintSuggestions   - hlint hints to provide
# configHlintErrors        - hlint hints to enforce
# configGhcWarnings        - GHC warnings to provide as hints
# configGhcErrors          - GHC warnings to enforce
----------
module Solution where

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
import Solution (Nat(..))
import qualified Solution
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
  [qcWithTimeoutAndRuns 50000 1000 $ \n1 n2 -> Solution.mult n1 n2 == mult n1 n2]

deriving instance Eq Nat

instance Arbitrary Nat where
   arbitrary = sized $ \n -> growingElements $ scanl (const . Succ) Zero [1..n]
