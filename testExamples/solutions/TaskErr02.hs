module Main where
import Control.Monad

addInput :: Int -> IO ()
addInput num = do
  line <- getLine'
  if (line /= "end") then do
    num' <- if isNum line 
              then return (num + (read line :: Int)) 
              else do putStrLn "Input is not a number!"
                      return num
    addInput num'
  else putStrLn $ "Result: " ++ (show num)

getLine' :: IO String
getLine' = do c <- getChar
              if c == '\n'
                  then return ""
                  else do l <- getLine'
                          return (c:l)

isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = elem x "1234567890" && isNum xs