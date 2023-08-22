enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- name-shadowing

#{commonConfigHlintErrors}
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Redundant return
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
- Use let
- Use odd
- Use print
- Use putStr
- Use putStrLn
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds
- unused-do-bind
- wrong-do-bind

#{commonConfigHlintRules}
- 'warn: {lhs: IOTasks.IOrep.putStrLn (show x), rhs: IOTasks.IOrep.print x}'
- 'warn: {lhs: IOTasks.IOrep.putStr (x ++ "\n"), rhs: IOTasks.IOrep.putStrLn x}'
- 'warn: {lhs: IOTasks.IOrep.putStr (x ++ y ++ "\n"), rhs: IOTasks.IOrep.putStrLn (x ++ y)}'
- 'warn: {lhs: mapM_ IOTasks.IOrep.putChar, rhs: IOTasks.IOrep.putStr}'
- 'hint: {lhs: IOTasks.IOrep.print s, rhs: IOTasks.IOrep.putStrLn s, side: isLitString s, name: Consider avoiding print on String}'
- 'hint: {lhs: IOTasks.IOrep.print (s ++ t), rhs: IOTasks.IOrep.putStrLn (s ++ t), side: isLitString s || isLitString t, name: Consider avoiding print on String}'
- 'hint: {lhs: IOTasks.IOrep.print (s ++ t ++ u), rhs: IOTasks.IOrep.putStrLn (s ++ t ++ u), side: not (isLitString s) && (isLitString t || isLitString u), name: Consider avoiding print on String}'

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Consider avoiding print on String
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Redundant do
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
- Use foldM
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
- Use unless
- Use when

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Prelude hiding (IO, getChar, getLine, readLn,     -- remove this line to test locally
                       putChar, putStr, putStrLn, print) -- remove this line to test locally
import IOTasks.IOrep                                     -- remove this line to test locally
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

{- Write a program which first reads a positive integer n from the
 - console, and then reads n integers one after the other and finally
 - outputs their sum. Your program should prompt appropriately for its
 - inputs (indicating which number of summand is to be input next), as
 - well as explain its final output. These text prompts/outputs are
 - not optional.
 -}

main :: IO ()
main = undefined

{- If you hand in a wrong solution, Autotool might throw a rather
 - intimidating error message at you. At the top of this message you
 - will find a sequence of input values for which your program behaves
 - differently than what the task requires. This should hopefully be
 - enough to fix your mistake, even if you can't make sense of the
 - rest of the error.
 -}
----------
{-# LANGUAGE TypeApplications #-}
module Test (test) where
import Prelude hiding (getChar, getLine, readLn, putChar, putStr, putStrLn, print, all)
import qualified #{moduleName}
import Test.HUnit ((~:), Test, Assertion, assertFailure)
import IOTasks

import TestHelper (tcWithTimeoutAndArgs)

test :: [ Test ]
test =
  [ " correct program behavior?" ~:
    -- should work without maxSize parameter (defaults to 100) but takes longer to run
    tcWithTimeoutAndArgs 5000000 stdArgs {verbose = False}
      #{moduleName}.main
      specification
  ]

specification :: Specification
specification =
  arbitraryOutput <>
  readInput n pos AssumeValid <>
  tillExit (
    branch (currentValue n .==. length' (as @[Integer] $ allValues x))
      -- then
      exit
      -- else
      (writeOutput [Wildcard <> Value (length' (as @[Integer] $ allValues x) .+. intLit 1) <> Wildcard] <>
      readInput x ints AssumeValid
      )
  ) <>
  writeOutput [Wildcard <> Value (sum' $ allValues x) <> Wildcard]
  where
    n = intVar "n"
    x = intVar "x"

ints, pos :: ValueSet Integer
ints = Every
pos = GreaterThan 0
