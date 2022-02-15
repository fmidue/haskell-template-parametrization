{-|
Module      : Postprocessor

Contains function that is applied after the task generation.
-}

module Postprocessor ( whitespaceWatermarking ) where

import Data.List (dropWhileEnd, isPrefixOf)
import Data.Char (isSpace)
import Seed ( Seed(..) )

-- | Adds whitespaces to the end of a comment 
whitespaceWatermarking :: String -- ^ the task as string
                       -> Seed   -- ^ seed is used to generate whitespaces
                       -> String
whitespaceWatermarking str (Seed s) = unlines $ mark (lines str) s
   where mark [] _ = []
         mark content [] = content
         mark (x:xs) (y:ys) = if "--" `isPrefixOf` cropped && not ("---" `isPrefixOf` cropped) then
                                 if even y then (cropped ++ " "):mark xs (ys ++ [y]) else cropped:mark xs (ys ++ [y])
                              else cropped:mark xs (y:ys)
                                 where cropped = dropWhileEnd isSpace x
