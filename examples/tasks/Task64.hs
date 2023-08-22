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
- unused-do-bind
- wrong-do-bind

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
- Redundant do
- Redundant if
- Redundant return
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
- Use let
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
- Use print
- Use putStr
- Use putStrLn
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tuple-section
# - Use uncurry
- Use unless
- Use when
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings: []

#{commonConfigHlintRules}
- 'warn: {lhs: IOTasks.IOrep.putStrLn (show x), rhs: IOTasks.IOrep.print x}'
- 'warn: {lhs: IOTasks.IOrep.putStr (x ++ "\n"), rhs: IOTasks.IOrep.putStrLn x}'
- 'warn: {lhs: IOTasks.IOrep.putStr (x ++ y ++ "\n"), rhs: IOTasks.IOrep.putStrLn (x ++ y)}'
- 'warn: {lhs: mapM_ IOTasks.IOrep.putChar, rhs: IOTasks.IOrep.putStr}'
- 'hint: {lhs: IOTasks.IOrep.print s, rhs: IOTasks.IOrep.putStrLn s, side: isLitString s, name: Consider avoiding print on String}'
- 'hint: {lhs: IOTasks.IOrep.print (s ++ t), rhs: IOTasks.IOrep.putStrLn (s ++ t), side: isLitString s || isLitString t, name: Consider avoiding print on String}'
- 'hint: {lhs: IOTasks.IOrep.print (s ++ t ++ u), rhs: IOTasks.IOrep.putStrLn (s ++ t ++ u), side: not (isLitString s) && (isLitString t || isLitString u), name: Consider avoiding print on String}'

#{commonConfigHlintSuggestions}
- Consider avoiding print on String
- Use foldM
- Use foldl
- Use tail

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

{- Write a program that reads in integers (negative integers too)
 - until the two most recently entered integers sum up to zero. Once
 - this happens, the program should output the amount of integers read
 - in (including the last two) which are larger than zero and
 - divisible by three.
 -
 - Note that your program always has to read in at least two integers
 - before it can terminate.
 -
 - An example: For the inputs 4, 3, -3 the correct output would be 1.
 - The program terminates after -3, because 3 + (-3) = 0; and the
 - output is 1, because only 3 is both larger than zero and divisible
 - by three.
 -
 - You can add additional information to outputs throughout, possibly
 - also to indicate what the user has to do next.
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
import Prelude hiding (getChar, getLine, readLn, putChar, putStr, putStrLn, print, all)
import qualified #{moduleName}
import Test.HUnit ((~:), Test, Assertion, assertFailure)
import IOTasks

import TestHelper (tcWithTimeoutAndArgs)

test :: [ Test ]
test =
  [ " correct program behavior?" ~:
    tcWithTimeoutAndArgs 10000000 stdArgs {verbose = False}
      #{moduleName}.main
      specification
  ]

specification :: Specification
specification =
  optionalTextOutput <>
  readInput x ints AssumeValid <>
  optionalTextOutput <>
  readInput y ints AssumeValid <>
  tillExit (
    branch (currentValue x .+. currentValue y .==. intLit 0)
      exit
      (optionalTextOutput <>
       readInput x ints AssumeValid <>
        branch (currentValue x .+. currentValue y .==. intLit 0)
          exit
          (optionalTextOutput <>
           readInput y ints AssumeValid)
    )
  ) <>
  writeOutput [Wildcard <> Value (length' $ filter' predicate $ allValues [x,y]) <> Wildcard]
  where
    predicate x = x > 0 && x `mod` 3 == 0
    x = intVar "x"
    y = intVar "y"

ints, nats :: ValueSet Integer
ints = Every
nats = Eq 0 `Union` GreaterThan 0
