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
main = do hSetBuffering stdout NoBuffering
          putStr "Please enter the number (a positive integer) of summands: "
          n <- readLn
          loop n 1 0

loop :: Integer -> Integer -> Integer -> IO ()
loop n i s | n < i = do putStrLn "Input finished."
                        let pluralize = if i == 2 then "One summand was"
                                                  else show (i-1) ++ " summands were"
                        putStrLn $ pluralize ++ " entered."
                        putStrLn $ "The sum is: " ++ show s ++ "."
loop n i s = do putStr $ "Please enter the " ++ show i ++ ". summand: "
                z <- readLn
                loop n (i+1) (s+z)

{- If you hand in a wrong solution, Autotool might throw a rather
 - intimidating error message at you. At the top of this message you
 - will find a sequence of input values for which your program behaves
 - differently than what the task requires. This should hopefully be
 - enough to fix your mistake, even if you can't make sense of the
 - rest of the error.
 -}
