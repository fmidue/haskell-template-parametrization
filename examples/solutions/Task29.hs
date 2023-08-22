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

{- Write a program that reads in two integers (negative integers too)
 - and prints out their sum. This behavior is repeated until the first
 - of the two numbers read is 0. The program then terminates (not
 - reading a second number), printing the count of additions
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
main = loop 0

loop :: Integer -> IO ()
loop n = do
  putStr "First number or 0 to exit: "
  x <- readLn
  if x == 0
    then do
      putStrLn "Exiting program"
      putStr "The number of additions performed was: "
      print n
    else do
      putStr "Second number: "
      y <- readLn
      putStr ("The sum of " ++ show x ++ " and " ++ show y ++ " is ")
      print (x + y)
      loop (n + 1)
