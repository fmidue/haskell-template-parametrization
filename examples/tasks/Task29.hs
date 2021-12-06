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
- unused-do-bind
- wrong-do-bind
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
- Redundant do
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
- Redundant return
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
- Use let
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
- Use print
- Use product
- Use putStr
- Use putStrLn
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
- Use unless
- Use when
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
- 'warn: {lhs: Test.IOTasks.IOrep.putStrLn (show x), rhs: Test.IOTasks.IOrep.print x}'
- 'warn: {lhs: Test.IOTasks.IOrep.putStr (x ++ "\n"), rhs: Test.IOTasks.IOrep.putStrLn x}'
- 'warn: {lhs: Test.IOTasks.IOrep.putStr (x ++ y ++ "\n"), rhs: Test.IOTasks.IOrep.putStrLn (x ++ y)}'
- 'warn: {lhs: mapM_ Test.IOTasks.IOrep.putChar, rhs: Test.IOTasks.IOrep.putStr}'
- 'hint: {lhs: Test.IOTasks.IOrep.print s, rhs: Test.IOTasks.IOrep.putStrLn s, side: isLitString s, name: Consider avoiding print on String}'
- 'hint: {lhs: Test.IOTasks.IOrep.print (s ++ t), rhs: Test.IOTasks.IOrep.putStrLn (s ++ t), side: isLitString s || isLitString t, name: Consider avoiding print on String}'
- 'hint: {lhs: Test.IOTasks.IOrep.print (s ++ t ++ u), rhs: Test.IOTasks.IOrep.putStrLn (s ++ t ++ u), side: not (isLitString s) && (isLitString t || isLitString u), name: Consider avoiding print on String}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Avoid lambda using `infix`
- Consider avoiding print on String
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
- Use foldM
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
import Prelude hiding (IO, getChar, getLine, readLn,     -- remove this line to test locally
                       putChar, putStr, putStrLn, print) -- remove this line to test locally
import Test.IOTasks.IOrep                                -- remove this line to test locally
type IO = IOrep                                          -- remove this line to test locally

{- In the following IO programming task, you can use the primitives
 - listed above, as well as 'return'. If you want to test your
 - submission locally, simply remove all the marked lines above. But
 - for Autotool to accept your submission, those lines should be put
 - back in, exactly as they are above.
 -
 - Moreover, if you first want to test locally, then depending on your
 - operating system and settings, you might have to additionally add
 - an 'import System.IO' and start your program in 'main' with the
 - statement 'hSetBuffering stdout NoBuffering'. (In Autotool that
 - statement has no effect and can be omitted, but does not hurt
 - either. However, the potentially added line 'import System.IO' has
 - to be removed before uploading to Autotool.)
 -}

{- Write a program that reads in two integers (negative integers too)
 - and prints out their sum. This behavior is repeated until the first
 - of the two numbers read is 0. The program then terminates (not
 - reading a second number), printing the number of additions
 - performed.
 -
 - You can add additional information to both the output of the
 - addition results as well as the final output. Furthermore you might
 - want to add additional outputs to indicate what the user has to do
 - next.
 -}

{- If you hand in a wrong solution, Autotool might throw a rather
 - intimidating error message at you. At the top of this message you
 - will find a sequence of input values for which your program behaves
 - differently than what the task requires. This should hopefully be
 - enough to fix your mistake, even if you can't make sense of the
 - rest of the error.
 -}

main :: IO ()
main = undefined
----------
{-# LANGUAGE TypeApplications #-}
module Test (test) where
import Prelude hiding (getChar, getLine, readLn, putChar, putStr, putStrLn, print)
import qualified Solution
import Test.QuickCheck
import Test.HUnit ((~:), Test, Assertion, assertFailure)
import Test.IOTasks
import Data.Term.PTerm

import TestHelper (qcWithTimeoutAndArgs)

test :: [ Test ]
test =
  [ " correct program behavior?" ~:
    -- should work without maxSize parameter (defaults to 100) but takes longer to run
    qcWithTimeoutAndArgs 5000000 stdArgs{maxSuccess=250, maxSize=70} $
      Solution.main `fulfillsClever` specification
  ]

type Term = PTerm Varname

specification :: Specification Term
specification =
  tillExit (
    optionalTextOutput <>
    readInput "x" ints <>
    branch (getCurrent @Int "x" :== Lit 0)
    (optionalTextOutput <>
     readInput "y" ints <>
     writeOutput [anything <> var 0 <> anything] [getCurrent @Int "x" :+ getCurrent @Int "y"])
    exit
  ) <>
  writeOutput [anything <> var 0 <> anything] [Length $ getAll @Int "y"]

optionalTextOutput :: Specification Term
optionalTextOutput = optional (writeFixedOutput [anything])
