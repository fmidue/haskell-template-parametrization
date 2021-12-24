module Default ( seed, with ) where

import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Data.Time.Calendar ( toGregorian )
import Data.Functor ( (<&>) )
import Task (Task, addSimpleVar, containsVar)

seed :: Task -> IO Task
seed task = do
    (year, month, _) <- getCurrentTime <&> (toGregorian . utctDay)
    let s = if month > 2 && month < 9 then "summer of " ++ show year else "winter of " ++ show year
    let name = "seed"
    return $ if containsVar name task then task else addSimpleVar ("seed", s) task

with :: Task -> [Task -> IO Task] -> IO Task
with task [] = return task
with task (f:fs) = do
    task' <- with task fs
    f task'