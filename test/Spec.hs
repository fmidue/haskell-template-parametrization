--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec (hspec, describe, it, shouldNotSatisfy)
import System.IO (openTempFile, stderr)
import System.Random ( newStdGen, Random(randomRs, randomRIO), StdGen )
import GHC.IO.Handle ( hFlush, hClose, hPutStr )
import Control.Monad ( forM, forM_ )
import Data.Array.IO
    ( newListArray, readArray, writeArray, IOArray )
import Data.List ( (\\) )
import Text.Parsec (parse)
import Task (exercise, Task)
import Text.Printf (printf)
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
        describe "parse task" $ do
                it "should not throw any exception" $ do
                        r <- buildRandomTask
                        parse exercise "" r `shouldNotSatisfy` isParseError

isParseError :: Either a b -> Bool
isParseError (Left err) = True 
isParseError _ = False 

buildRandomTask :: IO String
buildRandomTask = do
        let g = newStdGen
        names <- sequence [randString g 5 | _ <- [0..10]]
        vars <- buildVars names names
        sep <- separator g
        code <- buildContent names
        tests <- buildContent names
        let output = vars ++ [sep] ++ code ++ [sep] ++ tests
        return (unlines output)

parseStrings :: IO ()
parseStrings = do
        let parserTests = 25
        forM_ [1..parserTests] (`parseRandomString` parserTests)
        putStrLn "\n25 test completed without any error."

parseRandomString :: Int -> Int -> IO ()
parseRandomString progress max = do
        output <- buildRandomTask
        case parse exercise "" output of
                Left err -> error $ show err
                Right ex -> hPutStr stderr $ "\r\ESC[K" ++ show progress ++ "/" ++ show max ++ " completed."

randString :: IO StdGen -> Int -> IO String
randString g length = fmap (take length . randomRs ('a','z')) g

buildVars :: [String] -> [String] -> IO [String]
buildVars [] _ = return []
buildVars (x:xs) allNames = do
        vars <- buildVars xs allNames
        rnames <- shuffle (allNames \\ (x:xs))
        amount <- randomRIO (0, length rnames) :: IO Int
        n <- randomRIO (0, 1) :: IO Int
        if n == 1 then return $ buildSingleLineVar x (take amount rnames) : vars else return $ buildMultiLineVar x (take amount rnames) : vars

buildSingleLineVar :: String -> [String] -> String
buildSingleLineVar name vars = name ++ " = return \"TEST " ++ concatMap (\ x -> "#{" ++ x ++ "}") vars ++"\""

buildMultiLineVar :: String -> [String] -> String
buildMultiLineVar name vars = name ++ " {\nSome\nmultiline\ntest\n" ++ concatMap (\ x -> "#{" ++ x ++ "}\n") vars ++ "}"

buildNameList :: [String] -> IO [String]
buildNameList [] = return []
buildNameList (x:xs) = do
        n <- randomRIO (0, 5) :: IO Int
        nameList <- buildNameList xs
        return $ replicate n x ++ nameList

buildContent :: [String] -> IO [String]
buildContent names = do
        tmp <- buildNameList names
        nameList <- shuffle tmp
        return $ map (\ x -> "#{" ++ x ++ "}") nameList

separator :: IO StdGen -> IO String
separator g = do
        n <- randomRIO (0, 25) :: IO Int
        text <- randString g n
        return $ "---" ++ text

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs