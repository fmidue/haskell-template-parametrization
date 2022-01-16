module Fileloader ( loadAllTasks ) where

import Task ( combineToString, parseTask )
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath.Posix ( takeDirectory, takeFileName )
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Default (with, seed, enableWhitespaceWatermarking)
import Postprocessor (whitespaceWatermarking)
import Seed (stringToSeed)

loadAllTasks :: FilePath -> IO ()
loadAllTasks folder = do
    let tasksFolder = folder ++ "/tasks"
    let solutionsFolder = folder ++ "/solutions"
    createDirectoryIfMissing True $ takeDirectory "output/tasks/"
    createDirectoryIfMissing True $ takeDirectory "output/solutions/"
    defaults           <- readFile $ folder ++ "/defaults.hs"
    defaultTask        <- parseTask defaults
    (_, defaultVars)   <- combineToString defaultTask True M.empty
    tasks              <- getDirectoryContents tasksFolder
    solutions          <- getDirectoryContents solutionsFolder
    let allFiles = [(tasksFolder ++ "/" ++ task, solutionsFolder ++ "/" ++ solution) | task <- tasks, solution <- solutions, task == solution, ".hs" `isSuffixOf` task]
    evalTasksAndSolutions defaultVars allFiles


evalTasksAndSolutions :: M.Map String String -> [(FilePath, FilePath)] -> IO ()
evalTasksAndSolutions _ [] = print "Converted all Files!"
evalTasksAndSolutions defaults ((x, y):xs) = do 
    tFileContent          <- readFile x
    task                  <- parseTask tFileContent
    task'                 <- with task defaults [seed, enableWhitespaceWatermarking]
    (taskOutput, taskMap) <- combineToString task' False defaults
    sFileContent          <- readFile y
    solution              <- parseTask sFileContent
    (solutionOutput, _)   <- combineToString solution False taskMap
    writeFile ("output/tasks/" ++ takeFileName x) (if taskMap M.! "enableWhitespaceWatermarking" == "True" then whitespaceWatermarking taskOutput (stringToSeed (taskMap M.! "seed")) else taskOutput)
    writeFile ("output/solutions/" ++ takeFileName x) solutionOutput
    evalTasksAndSolutions defaults xs