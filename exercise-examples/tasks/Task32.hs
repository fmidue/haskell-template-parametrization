enableWhitespaceWatermarking = return "True"
moduleName = return "Task32"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
# - name-shadowing # allow shadowing, to permit certain C/Python-like versions
- unused-local-binds
- unused-matches
- unused-pattern-binds
- unused-do-bind
- wrong-do-bind

#{commonConfigHlintErrors}
- Avoid lambda
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant do
- Redundant if
- Redundant return
- Use &&
- "Use :"
- Use all
- Use and
- Use any
- Use camelCase
- Use concatMap
- Use const
# - Use even # gibt es in C/Python ja auch nicht
- Use guards
- Use if
- Use let
- Use notElem
# - Use odd # gibt es in C/Python ja auch nicht
- Use or
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
- missing-signatures

#{commonConfigHlintRules}
- 'warn: {lhs: Test.IOSpec.putStrLn (show x), rhs: IOTestHelper.print x}'
- 'warn: {lhs: Test.IOSpec.putStr (x ++ "\n"), rhs: Test.IOSpec.putStrLn x}'
- 'warn: {lhs: Test.IOSpec.putStr (x ++ y ++ "\n"), rhs: Test.IOSpec.putStrLn (x ++ y)}'
- 'warn: {lhs: mapM_ Test.IOSpec.putChar, rhs: Test.IOSpec.putStr}'
- 'hint: {lhs: IOTestHelper.print s, rhs: Test.IOSpec.putStrLn s, side: isLitString s, name: Consider avoiding print on String}'
- 'hint: {lhs: IOTestHelper.print (s ++ t), rhs: Test.IOSpec.putStrLn (s ++ t), side: isLitString s || isLitString t, name: Consider avoiding print on String}'
- 'hint: {lhs: IOTestHelper.print (s ++ t ++ u), rhs: Test.IOSpec.putStrLn (s ++ t ++ u), side: not (isLitString s) && (isLitString t || isLitString u), name: Consider avoiding print on String}'

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Consider avoiding print on String
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Use ++
- Use 1
- Use catMaybes
- Use concat
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
# - Use null
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry

#{commonConfigLanguageExtensions}
- FlexibleContexts # because of IOSpec types
----------
module #{moduleName} where
import Prelude hiding (IO, getChar, getLine, readLn,
                       putChar, putStr, putStrLn, print)
import Test.IOSpec
import IOTestHelper
type IO = IOSpec Teletype

{- In the following IO programming task, you can use the primitives
 - listed above, as well as 'return'. If you want to test your
 - submission locally, simply remove all the above lines. But for
 - Autotool to accept your submission, those lines should be put back
 - in, exactly as they are above.
 -
 - Moreover, if you first want to test locally, then depending on your
 - operating system and settings, you might have to additionally add
 - an 'import System.IO' and start your program in 'main' with the
 - statement 'hSetBuffering stdout NoBuffering'. (In Autotool that
 - statement has no effect and can be omitted, but does not hurt
 - either. However, the potentially added line 'import System.IO' has
 - to be removed before uploading to Autotool.)
 -}

{- Write a program that behaves *exactly* like the following
 - imperative programs. That is, your Haskell program should cover
 - exactly the same input/output interactions as this C or Python
 - program, respectively:

C version:
==========

void main() {
  int x, y;
  scanf("%d", &x);
  y = 0;
  while (x > 0) {
    if (x % 2 == 0)
      x = x / 3;
    else {
      scanf("%d", &x);
      y = y + 1;
    }
    printf("%d\n", x);
  }
  printf("%d\n", y);
}

Python version:
===============

def main():
  x = int(input())
  y = 0
  while (x > 0):
    if (x % 2 == 0):
      x = x // 3
    else:
      x = int(input())
      y = y + 1
    print(x)
  print(y)

-}

-- You can assume that the user will never enter negative numbers.

main :: IO ()
main = undefined
----------
{-# LANGUAGE StandaloneDeriving #-}
module Test (test) where
import Prelude hiding (getChar, getLine, readLn, putChar, putStr, putStrLn, print)
import qualified #{moduleName}
import Test.QuickCheck
import IOTestHelper
import TestHelper (qcWithTimeoutAndRuns)
import Test.HUnit ((~:), Test, Assertion, assertFailure)
import Data.Maybe
import Control.Monad
import Test.IOSpec.Types
import Test.IOSpec.Teletype

{- data Dialog = Input [String] Dialog | Output String Dialog | Finished -}

deriving instance Eq Dialog

data Case = Case [Int] (Maybe Dialog) Dialog

instance Show Case where
  show (Case _ _ correct) = "Program does not cover (exactly) the following interaction:\n"
                            ++ "(here an example for correct behavior,"
                            ++ " from Autotool-internal solution)\n"
                            ++ show (split correct)
    where split (Input xss diag) = Input xss (split diag)
          split (Output xs diag) = foldr (Output . (++"\n")) (split diag) (lines xs)
          split Finished         = Finished

test :: [ Test ]
test =
  [ " correct program behavior?" ~:
      qcWithTimeoutAndRuns 5000000 250
         $ forAll (inputs >>= \ns -> return $ Case ns (makeDialog #{moduleName}.main
                                                       (makeInputs ns))
                                                      (correctDialog ns))
         $ \(Case _ actual correct) -> maybe False (correct ==) actual
  ]

inputs :: Gen [Int]
inputs = do ns <- listOf (arbitrary `suchThat` (\(Positive n') -> not . ends $ n'))
            n <- sized ending
            return $ map (\(Positive n') -> n') ns ++ [n]
  where ends n = n==0 || even n && ends (n `div` 3)
        ending s = frequency [(1, return 0),
                              (s, do n <- ending (s `div` 2)
                                     liftM (3*n+) (if odd n then return 1
                                                            else elements [0,2]))]

correctDialog :: [Int] -> Dialog
correctDialog = fromJust . makeDialog main . makeInputs

main :: IOSpec Teletype ()
main = do x <- readLn
          loop x 0
  where loop 0 y = print y
        loop x y | even x = do let x' = x `div` 3
                               print x'
                               loop x' y
        loop _ y          = do x <- readLn
                               print x
                               loop x (y+1)
----------
module IOTestHelper (readLn, print, Dialog(..), makeInputs, makeDialog,
                     hSetBuffering, stdout, BufferMode (NoBuffering),
                     subStringOf) where
import Prelude hiding (IO, getLine, readLn, putStrLn, print)
import Test.IOSpec.Types
import Test.IOSpec.Teletype
import Test.IOSpec.VirtualMachine

subStringOf :: String -> String -> Bool
subStringOf _  "" = False
subStringOf xs ys = xs == take (length xs) ys || subStringOf xs (tail ys)

readLn :: Read a => IOSpec Teletype a
readLn = getLine >>= (return . read)

print :: Show a => a -> IOSpec Teletype ()
print x = putStrLn (show x)

data Handle = StdOut

stdout :: Handle
stdout = StdOut

data BufferMode = NoBuffering

hSetBuffering :: Handle -> BufferMode -> IOSpec Teletype ()
hSetBuffering StdOut NoBuffering = return ()

data Dialog = Input [String] Dialog | Output String Dialog | Finished

{- invariants:
 -  * immer abwechselnd Input/Output
 -  * in Input, nur nichtleere Listen, darin Strings nur am Ende '\n', und alle
 -    nicht-letzten auf jeden Fall so beendet
 -  * obige Regel (bis auf Nichleerheit) auch fuer zweites makeDialog-Argument
 -  * only give "allowed" input sequences to makeDialog
 -}

instance Show Dialog where
  show Finished        = ""
  show (Output s diag) = "Output: " ++ show s ++ "\n" ++ show diag
  show (Input xs diag) = unlines (map (\s -> "Input: " ++ show s) xs) ++ show diag

makeInputs :: Show a => [a] -> [String]
makeInputs = map ((++"\n") . show)

makeDialog ::  IOSpec Teletype () -> [String] -> Maybe Dialog
makeDialog = makeDialog' . flip evalIOSpec singleThreaded

makeDialog' :: Effect () -> [String] -> Maybe Dialog
makeDialog' (Fail s) _ = error $ "impossible: " ++ s
makeDialog' (Done ()) []   = Just Finished
makeDialog' (Done ()) [""] = Just Finished
makeDialog' (Done ()) _    = Nothing
makeDialog' (ReadChar _) []   = Nothing
makeDialog' (ReadChar _) [""] = Nothing
makeDialog' (ReadChar _) ("":_:_) = error "impossible!"
makeDialog' (ReadChar f) ("\n":xss) = makeDialog' (f '\n') xss >>=
                                      \m -> case m of
                                              Input yss diag
                                                -> return $ Input ("\n":yss) diag
                                              other -> return $ Input ["\n"] other
makeDialog' (ReadChar _) (('\n':_):_) = error "impossible!"
makeDialog' (ReadChar f) ((c:xs):xss) = makeDialog' (f c)
                                        (case xs of {"" -> case xss of {[] -> []; _ -> error "impossible!"};
                                                     _ -> xs:xss}) >>=
                                        \m -> case m of
                                                Input (ys:yss) diag
                                                  -> return $ Input ((c:ys):yss) diag
                                                other -> return $ Input [[c]] other
makeDialog' (Print c e) xss = makeDialog' e xss
                              >>= \m -> case m of Output ys diag
                                                    -> return $ Output (c:ys) diag
                                                  other -> return $ Output [c] other
