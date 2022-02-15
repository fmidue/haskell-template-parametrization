module Main where
import System.Environment (getArgs)
import Fileloader (loadAllTasks)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn $ "\ESC[91mMissing argument! Please specify a folder.\ESC[0m"
    x : xs ->  loadAllTasks x
 
