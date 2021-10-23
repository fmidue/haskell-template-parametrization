module ExerciseData (staticValues, toMap, splitData) where
import qualified Data.Map as M
import Data.List (isPrefixOf)

toMap :: [String] -> M.Map String String
toMap [] = M.empty
toMap (x:xs) = M.insert key value (toMap xs)
            where (key, _:value) = break (=='=') (filter (/=' ') x)

staticValues :: [String] -> [String]
staticValues xs = let (_:ys) = dropWhile (not . isPrefixOf "## Static") xs in takeWhile (not . isSep') ys

splitData :: String -> ([String], [String])
splitData xs = let (dat, _:rest) = break isSep (lines xs) in (dat, rest)

isSep' :: String -> Bool
isSep' ('#':'#':_) = True
isSep' _ = False

isSep :: String -> Bool
isSep ('-':'-':'-':_) = True
isSep _ = False