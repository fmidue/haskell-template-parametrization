module Main where
import System.IO

-- Write an IO program that reads in three integers (negative integers
-- too) and prints out the #{wordingWatermark} of them. This behavior is repeated
-- until the first new number read is 0. The program then immediately
-- terminates (not even reading a new second or third number) after
-- printing a count of completed iterations (that is the count of
-- maximums printed).
--
-- You can add additional information to both the output of the
-- maximum/comparison results as well as the final output. Moreover,
-- you might want to add additional outputs to indicate throughout
-- what the user has to do next.

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          loop 0

loop :: Integer -> IO ()
loop n = do
  putStr "First number or 0 to exit: "
  x <- readLn
  if x == 0
    then do
      putStrLn "Exiting program"
      putStr "The number of iterations performed was: "
      print n
    else do
      putStr "Second number: "
      y <- readLn
      putStr "Third number: "
      z <- readLn
      putStr ("The maximum of " ++ show x ++ ", " ++ show y ++ " and " ++ show z ++ " is ")
      print (maximum [x,y,z])
      loop (n + 1)
