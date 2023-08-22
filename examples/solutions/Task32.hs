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
main = do hSetBuffering stdout NoBuffering
          x <- readLn
          loop x 0
  where loop 0 y = print y
        loop x y | even x = do let x' = x `div` 3
                               print x'
                               loop x' y
        loop _ y          = do x <- readLn
                               print x
                               loop x (y+1)
