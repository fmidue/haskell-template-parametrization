module Default ( seed, with ) where

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
    return $ if containsVar name task || name `elem` M.keys defaults then task else addSimpleVar ("seed", seedToString $ stringToSeed s) task

with :: Task -> M.Map String String -> [Task -> M.Map String String -> IO Task] -> IO Task
with task _ [] = return task
with task def (f:fs) = do
    task' <- with task def fs
    f task' def