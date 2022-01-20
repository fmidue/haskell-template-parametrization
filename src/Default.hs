module Default ( seed, enableWhitespaceWatermarking, defaultImports, withCurrentSeed, withSeed, defaultFunctions, with ) where

import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Data.Time.Calendar ( toGregorian )
import Data.Functor ( (<&>) )
import Task (Task, addSimpleVar, containsVar, addRawVar, Part (Rest, Placeholder))
import Seed (stringToSeed, seedToString)
import qualified Data.Map as M

seed :: Task -> M.Map String String -> IO Task
seed task defaults = do
    (year, month, _) <- getCurrentTime <&> (toGregorian . utctDay)
    let s = if month > 2 && month < 9 then "summer of " ++ show year else "winter of " ++ show year
    let name = "seed"
    return $ addToTask name task (addSimpleVar (name, seedToString $ stringToSeed s)) defaults

defaultFunctions :: Task -> M.Map String String -> IO Task
defaultFunctions task defaults = do
    let name = "plain_defaultFunctions"
    return $ addToTask name task (addRawVar (name, [Placeholder "plain_withCurrentSeed", Rest "\n", Placeholder "plain_withSeed", Rest "\n"])) defaults

defaultImports :: Task -> M.Map String String -> IO Task
defaultImports task defaults = do
    let name = "plain_defaultImports"
    return $ addToTask name task (addRawVar (name, [Rest "import Data.List (isPrefixOf, isSuffixOf)\nimport Test.QuickCheck.Gen\nimport Test.QuickCheck.Random (mkQCGen)\n"])) defaults

withCurrentSeed :: Task -> M.Map String String -> IO Task
withCurrentSeed task defaults = do
    let name = "plain_withCurrentSeed"
    return $ addToTask name task (addRawVar (name, [Rest "withCurrentSeed :: Show a => Gen a -> IO String\nwithCurrentSeed content = return $ if isPrefixOf \"\\\"\" str && isSuffixOf \"\\\"\" str then init (tail str) else str\n  where str = show (unGen ( content ) (mkQCGen ", Placeholder "seed", Rest ") 5)\n"])) defaults

withSeed :: Task -> M.Map String String -> IO Task
withSeed task defaults = do
    let name = "plain_withSeed"
    return $ addToTask name task (addRawVar (name, [Rest "withSeed :: Show a => Gen a -> Int -> IO String\nwithSeed content seed = return $ if isPrefixOf \"\\\"\" str && isSuffixOf \"\\\"\" str then init (tail str) else str\n  where str = show (unGen ( content ) (mkQCGen seed) 5)\n"])) defaults


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