{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Fileloader ( loadTasksFromFiles, loadAllTasks ) where

import Task ( loadTask, combineToString, parseTask )
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath.Posix ( takeDirectory, takeFileName )
import Data.List (isSuffixOf)
import qualified Data.Map as M

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
    tasks <- getDirectoryContents tasksFolder
    solutions <- getDirectoryContents solutionsFolder
    let allFiles = [(tasksFolder ++ "/" ++ task, solutionsFolder ++ "/" ++ solution) | task <- tasks, solution <- solutions, task == solution, ".hs" `isSuffixOf` task]
    evalTasksAndSolutions allFiles


evalTasksAndSolutions :: [(FilePath, FilePath)] -> IO ()
evalTasksAndSolutions [] = print "Converted all Files!"
evalTasksAndSolutions ((x, y):xs) = do 
    tFileContent <- readFile x
    task <- parseTask tFileContent
    (taskOutput, taskMap) <- combineToString task M.empty
    sFileContent <- readFile y
    solution <- parseTask sFileContent
    (solutionOutput, _) <- combineToString solution taskMap
    writeFile ("output/tasks/" ++ takeFileName x) taskOutput
    writeFile ("output/solutions/" ++ takeFileName x) solutionOutput
    evalTasksAndSolutions xs