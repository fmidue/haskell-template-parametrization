{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Fileloader ( loadTasksFromFiles ) where

import Task ( loadTask, combineToString )

loadTasksFromFiles :: FilePath -> IO () 
loadTasksFromFiles folder = do
    t <- combineToString [loadTask|tasks/task-test.hs|]
    writeFile (folder ++ "task-test.hs") t


