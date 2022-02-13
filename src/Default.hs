module Default ( seed, enableWhitespaceWatermarking, defaultImports, withCurrentSeed, withSeed, defaultFunctions, with ) where

import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Data.Time.Calendar ( toGregorian )
import Data.Functor ( (<&>) )
import Task (Task, addSimpleVar, containsVar, addRawVar, Part (Rest, Placeholder))
import Seed (stringToSeed, seedToString)
import qualified Data.Map as M
import Snippet (Snippet(Snippet))
import qualified DefaultSnippets as S
import Data.List (union)

seed :: Task -> M.Map String String -> IO Task
seed task defaults = do
    (year, month, _) <- getCurrentTime <&> (toGregorian . utctDay)
    let s = if month > 2 && month < 9 then "summer of " ++ show year else "winter of " ++ show year
    let name = "seed"
    return $ addToTask name task (addSimpleVar (name, seedToString $ stringToSeed s)) defaults

-- | Adds all default functions
defaultFunctions :: Task -> M.Map String String -> IO Task
defaultFunctions task defaults = do
    let name = "plain_defaultFunctions"
    return $ addToTask name task (addRawVar (name, [Placeholder "plain_withCurrentSeed", Rest "\n", Placeholder "plain_withSeed", Rest "\n"])) defaults

-- | Adds all default imports for every default function
defaultImports :: Task -> M.Map String String -> IO Task
defaultImports task defaults = do
    let name = "plain_defaultImports"
    return $ addToTask name task (addRawVar (name, [Rest (unlines (combineImports [S.withCurrentSeed, S.withSeed]))])) defaults

-- | Adds function to generate data with current seed
withCurrentSeed :: Task -> M.Map String String -> IO Task
withCurrentSeed task defaults = do
    let name = "plain_withCurrentSeed"
    return $ addToTask name task (addRawVar (name, let Snippet (_, code) = S.withCurrentSeed in asParts code)) defaults

-- | Adds function to generate data with a seed
withSeed :: Task -> M.Map String String -> IO Task
withSeed task defaults = do
    let name = "plain_withSeed"
    return $ addToTask name task (addRawVar (name, let Snippet (_, code) = S.withSeed in asParts code)) defaults

-- | Adds value to disable whitespace watermarking by default
enableWhitespaceWatermarking :: Task -> M.Map String String -> IO Task
enableWhitespaceWatermarking task defaults = do
    let name = "enableWhitespaceWatermarking"
    return $ addToTask name task (addSimpleVar (name, "False")) defaults

addToTask :: String -> Task -> (Task -> Task) -> M.Map String String -> Task
addToTask name task modifyTask defaults = if containsVar name task || name `elem` M.keys defaults then task else modifyTask task

combineImports :: [Snippet] -> [String]
combineImports [] = []
combineImports (x:xs) = let Snippet (imports, _) = x in union (combineImports xs) (lines imports)

asParts :: String -> [Part]
asParts [] = []
asParts str = let (a, ph, rest) = consume str in if null ph then Rest a : asParts rest else Rest a : Placeholder ph: asParts rest

consume :: String -> (String, String, String)
consume [] = ("", "", "")
consume ('#':'{':xs) = ("", placeholder, tail rest)
    where (placeholder, rest) = break ('}'==) xs
consume (x:xs) = let (a, p, rest) = consume xs in (x:a, p, rest)

with :: Task -> M.Map String String -> [Task -> M.Map String String -> IO Task] -> IO Task
with task _ [] = return task
with task def (f:fs) = do
    task' <- with task def fs
    f task' def