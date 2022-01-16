module Default ( seed, enableWhitespaceWatermarking, with ) where

import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Data.Time.Calendar ( toGregorian )
import Data.Functor ( (<&>) )
import Task (Task, addSimpleVar, containsVar)
import Seed (stringToSeed, seedToString)
import qualified Data.Map as M

seed :: Task -> M.Map String String -> IO Task
seed task defaults = do
    (year, month, _) <- getCurrentTime <&> (toGregorian . utctDay)
    let s = if month > 2 && month < 9 then "summer of " ++ show year else "winter of " ++ show year
    let name = "seed"
    return $ addToTask name task (addSimpleVar (name, seedToString $ stringToSeed s)) defaults

enableWhitespaceWatermarking :: Task -> M.Map String String -> IO Task
enableWhitespaceWatermarking task defaults = do
    let name = "enableWhitespaceWatermarking"
    return $ addToTask name task (addSimpleVar (name, "False")) defaults

addToTask :: String -> Task -> (Task -> Task) -> M.Map String String -> Task
addToTask name task modifyTask defaults = if containsVar name task || name `elem` M.keys defaults then task else modifyTask task

with :: Task -> M.Map String String -> [Task -> M.Map String String -> IO Task] -> IO Task
with task _ [] = return task
with task def (f:fs) = do
    task' <- with task def fs
    f task' def