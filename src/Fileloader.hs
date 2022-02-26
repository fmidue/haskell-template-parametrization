{-|
Module      : Fileloader
Description : Used to translate all files

Contains a simple function to load all files from a given
folder and replaces all placeholder with the given values.
-}

module Fileloader ( loadAllTasks ) where

import Task ( combineToString, parseTask )
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath.Posix ( takeDirectory, takeFileName )
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Default (with, seed, enableWhitespaceWatermarking, defaultImports, withCurrentSeed, defaultFunctions, withSeed)
import Postprocessor (whitespaceWatermarking)
import Seed (stringToSeed)

{- | Used to transform all given files from the target folder.
     There should be a default file 'defaults.hs' in the target folder.
     All task should be in the tasks folder. All solutions
     should be in the solutions folder.
-} 
loadAllTasks :: FilePath -- ^ target folder
             -> IO ()
loadAllTasks folder = do
    let tasksFolder = folder ++ "/tasks"
    let solutionsFolder = folder ++ "/solutions"
    createDirectoryIfMissing True $ takeDirectory "output/tasks/"
    createDirectoryIfMissing True $ takeDirectory "output/solutions/"
    defaults           <- readFile $ folder ++ "/defaults.hs"
    defaultTask        <- parseTask defaults
    defaultTask'       <- with defaultTask M.empty [defaultImports, defaultFunctions, seed, enableWhitespaceWatermarking, withCurrentSeed, withSeed]
    (_, defaultVars)   <- combineToString defaultTask' True M.empty
    tasks              <- getDirectoryContents tasksFolder
    solutions          <- getDirectoryContents solutionsFolder
    let allFiles = [(tasksFolder ++ "/" ++ task, solutionsFolder ++ "/" ++ solution) | task <- tasks, solution <- solutions, task == solution, ".hs" `isSuffixOf` task]
    evalTasksAndSolutions defaultVars allFiles


evalTasksAndSolutions :: M.Map String String -> [(FilePath, FilePath)] -> IO ()
evalTasksAndSolutions _ [] = print "Converted all Files!"
evalTasksAndSolutions defaults ((x, y):xs) = do 
    tFileContent          <- readFile x
    task                  <- parseTask tFileContent
    task'                 <- with task defaults [defaultImports, defaultFunctions, seed, enableWhitespaceWatermarking, withCurrentSeed, withSeed]
    (taskOutput, taskMap) <- combineToString task' True defaults
    sFileContent          <- readFile y
    solution              <- parseTask sFileContent
    (solutionOutput, _)   <- combineToString solution False taskMap
    writeFile ("output/tasks/" ++ takeFileName x) (if taskMap M.! "enableWhitespaceWatermarking" == "True" then whitespaceWatermarking taskOutput (stringToSeed (taskMap M.! "seed")) else taskOutput)
    writeFile ("output/solutions/" ++ takeFileName x) solutionOutput
    evalTasksAndSolutions defaults xs