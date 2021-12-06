{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Fileloader ( loadTasksFromFiles ) where

import Task ( loadTask, combineToString )
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

loadTasksFromFiles :: FilePath -> IO ()
loadTasksFromFiles folder = do
    t <- combineToString [loadTask|tasks/task-test.hs|]
    let path = folder ++ "task-test.hs"
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path t


