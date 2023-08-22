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
main = do putStr "Bitte geben Sie eine Zahl ein: "
          n <- readLn
          loop [n]

loop :: [Integer] -> IO ()
loop (m:ms) = do putStr "Naechste Zahl bitte: "
                 n <- readLn
                 if n+m == 0
                   then do putStrLn "Die Summe der letzten beiden Eingaben war 0."
                           putStr "Anzahl der durch drei teilbaren positiven Eingaben: "
                           print $ length $ filter (\n' -> (n' > 0) && (n' `mod` 3 == 0)) (n:m:ms)
                           putStrLn "Programm beendet."
                   else do putStrLn "Die Summe war noch nicht 0."
                           loop (n:m:ms)
loop [] = error "does not happen"
