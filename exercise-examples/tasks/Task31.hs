enableWhitespaceWatermarking = return "True"
moduleName = return "Task31"
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
 - submission locally, simply remove all the marked lines above, and
 - instead add an 'import System.IO' line. But for Autotool to accept
 - your submission, the lines should be restored exactly as they are
 - above.
 -}

-- Recall the definition of the function 'while' from the lecture:

while :: a -> (a -> Bool) -> (a -> IO a) -> IO a
while a p body = loop a
  where
    loop x = if p x
               then do x' <- body x
                       loop x'
               else return x

{- Using the above higher-order function, write a program which first
 - reads a positive integer n from the console, and then reads n
 - integers one after the other and finally outputs their product.
 - Your program should prompt appropriately for its inputs (indicating
 - which count of factor is to be input next), as well as explain its
 - final output. These text prompts/outputs are not optional.
 -
 - To solve the task, replace the occurrences of 'undefined' below.
 -
 - Do not introduce a recursive function of your own.
 -}

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Please enter the number (a positive integer) of factors: "
          n <- (readLn :: IO Int)
          returned <- while undefined undefined undefined
          undefined

{- If you hand in a wrong solution, Autotool might throw a rather
 - intimidating error message at you. At the top of this message you
 - will find a sequence of input values for which your program behaves
 - differently than what the task requires. This should hopefully be
 - enough to fix your mistake, even if you can't make sense of the
 - rest of the error.
 -}
----------
module Test (test) where
import Prelude hiding (getChar, getLine, readLn, putChar, putStr, putStrLn, print, all)
import qualified #{moduleName}
import Test.HUnit ((~:), Test, Assertion, assertFailure)
import IOTasks

import TestHelper (tcWithTimeoutAndArgs)

test :: [ Test ]
test =
  [ " correct program behavior?" ~:
    tcWithTimeoutAndArgs 5000000 stdArgs {verbose = False}
      #{moduleName}.main
      specification
  ]

specification :: Specification
specification =
  optionalTextOutput <>
  readInput "n" pos AssumeValid <>
  tillExit (
    branch (Current "n" :==: Length (All "x"))
      exit
      ( writeOutput [Wildcard <> Value (intLit 1 +# length' (all "x")) <> Wildcard] <>
        readInput "x" ints AssumeValid
      )
  ) <>
  writeOutput [Wildcard <> Value (product' (all "x")) <> Wildcard]

ints, pos :: ValueSet
ints = Every
pos = GreaterThan 0
