module Postprocessor ( whitespaceWatermarking ) where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Seed ( Seed(..) )



whitespaceWatermarking :: String -> Seed -> String
whitespaceWatermarking str (Seed s) = unlines $ mark (lines str) s

mark :: [String] -> [Int] -> [String]
mark [] _ = []
mark str [] = str
mark (x:xs) (y:ys) = if even y then (cropped ++ " "):mark xs (ys ++ [y]) else cropped:mark xs (ys ++ [y])
    where cropped = dropWhileEnd isSpace x
