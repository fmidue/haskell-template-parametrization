module Solution where
import Prelude hiding (IO, getChar, getLine, readLn,     -- remove this line to test locally
                       putChar, putStr, putStrLn, print) -- remove this line to test locally
import Test.IOTasks.IOrep                                -- remove this line to test locally
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
 - which number of factor is to be input next), as well as explain its
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
          returned <- while (1,1) (\(i,_) -> n >= i)
            (\(i,p) -> do
              putStr ("Enter factor " ++ show i ++ ": ")
              q <- readLn
              return (i+1, p*q)
            )
          putStrLn ("The product of the " ++ show n ++ " entered numbers is " ++ show (snd returned))

{- If you hand in a wrong solution, Autotool might throw a rather
 - intimidating error message at you. At the top of this message you
 - will find a sequence of input values for which your program behaves
 - differently than what the task requires. This should hopefully be
 - enough to fix your mistake, even if you can't make sense of the
 - rest of the error.
 -}
