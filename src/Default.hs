module Default ( seed, enableWhitespaceWatermarking, intGen, stringGen, intListGen, stringListGen, defaultImports, withCurrentSeed, defaultFunctions, with ) where

import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Data.Time.Calendar ( toGregorian )
import Data.Functor ( (<&>) )
import Task (Task, addSimpleVar, containsVar, addRawVar, addVar, Part (Rest, Placeholder))
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
    let name = "defaultFunctions"
    return $ addToTask name task (addVar (name, [Placeholder "withCurrentSeed", Rest "\\n"])) defaults

defaultImports :: Task -> M.Map String String -> IO Task
defaultImports task defaults = do
    let name = "defaultImports"
    return $ addToTask name task (addSimpleVar (name, "import Data.List (isPrefixOf, isSuffixOf)\\nimport Test.QuickCheck.Gen\\nimport Test.QuickCheck.Random (mkQCGen)\\n")) defaults

intGen :: Task -> M.Map String String -> IO Task
intGen task defaults = do
    let name = "int_gen"
    return $ addToTask name task (addRawVar (name, [Rest (name ++ " :: String\n" ++ name ++ " = show $ unGen ( chooseInt (-99, 99) ) (mkQCGen "), Placeholder "seed", Rest ") 0"])) defaults

stringGen :: Task -> M.Map String String -> IO Task
stringGen task defaults = do
    let name = "string_gen"
    return $ addToTask name task (addRawVar (name, [Rest (name ++ " :: String\n" ++ name ++ " = unGen ( elements [\"Test1\", \"Text2\", \"Text3\", \"Text4\", \"Text5\"] ) (mkQCGen "), Placeholder "seed", Rest ") 0"])) defaults

intListGen :: Task -> M.Map String String -> IO Task
intListGen task defaults = do
    let name = "intList_gen"
    return $ addToTask name task (addRawVar (name, [Rest (name ++ " :: String\n" ++ name ++ " = show $ unGen ( listOf1 $ chooseInt (-99, 99) ) (mkQCGen "), Placeholder "seed", Rest ") 5"])) defaults

stringListGen :: Task -> M.Map String String -> IO Task
stringListGen task defaults = do
    let name = "stringList_gen"
    return $ addToTask name task (addRawVar (name, [Rest (name ++ " :: String\n" ++ name ++ " = show $ unGen ( listOf1 $ elements [\"Test1\", \"Text2\", \"Text3\", \"Text4\", \"Text5\"] ) (mkQCGen "), Placeholder "seed", Rest ") 5"])) defaults

withCurrentSeed :: Task -> M.Map String String -> IO Task
withCurrentSeed task defaults = do
    let name = "withCurrentSeed"
    return $ addToTask name task (addVar (name, [Rest (name ++ " :: Show a => Gen a -> IO String\\\\n" ++ name ++ " content = return $ if isPrefixOf \\\\\\\"\\\\\\\\\\\\\\\"\\\\\\\" str && isSuffixOf \\\\\\\"\\\\\\\\\\\\\\\"\\\\\\\" str then init (tail str) else str\\\\n  where str = show (unGen ( content ) (mkQCGen "), Placeholder "seed", Rest ") 5)\\\\n"])) defaults


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