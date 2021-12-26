{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Fileloader ( loadTasksFromFiles, loadAllTasks ) where

import Task ( loadTask, combineToString, parseTask, loadAllVars )
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath.Posix ( takeDirectory, takeFileName )
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Default (with, seed)
import Postprocessor (whitespaceWatermarking)
import Seed (stringToSeed)

loadTasksFromFiles :: FilePath -> IO ()
loadTasksFromFiles folder = do
    (t,_) <- combineToString [loadTask|tasks/task-test.hs|] M.empty
    let path = folder ++ "task-test.hs"
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path t

loadAllTasks :: FilePath -> IO ()
loadAllTasks folder = do
    let tasksFolder = folder ++ "/tasks"
    let solutionsFolder = folder ++ "/solutions"
    createDirectoryIfMissing True $ takeDirectory "output/tasks/"
    createDirectoryIfMissing True $ takeDirectory "output/solutions/"
    defaults           <- readFile $ folder ++ "/defaults.hs"
    defaultTask <- parseTask defaults
    allVars <- (loadAllVars defaultTask M.empty)
    print allVars
    (_, defaultVars)   <- combineToString defaultTask M.empty
    tasks              <- getDirectoryContents tasksFolder
    solutions          <- getDirectoryContents solutionsFolder
    let allFiles = [(tasksFolder ++ "/" ++ task, solutionsFolder ++ "/" ++ solution) | task <- tasks, solution <- solutions, task == solution, ".hs" `isSuffixOf` task]
    evalTasksAndSolutions defaultVars allFiles


evalTasksAndSolutions :: M.Map String String -> [(FilePath, FilePath)] -> IO ()
evalTasksAndSolutions _ [] = print "Converted all Files!"
evalTasksAndSolutions defaults ((x, y):xs) = do 
    tFileContent <- readFile x
    task         <- parseTask tFileContent
    task'        <- task `with` [seed]
    (taskOutput, taskMap) <- combineToString task' defaults
    sFileContent <- readFile y
    solution <- parseTask sFileContent
    print taskMap
    (solutionOutput, _) <- combineToString solution taskMap
    writeFile ("output/tasks/" ++ takeFileName x) (whitespaceWatermarking taskOutput (stringToSeed (taskMap M.! "seed")))
    writeFile ("output/solutions/" ++ takeFileName x) solutionOutput
    evalTasksAndSolutions defaults xs