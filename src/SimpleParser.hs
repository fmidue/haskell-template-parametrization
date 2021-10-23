{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleParser (pTest) where

import Data.Text as T (pack, unpack, replace)
import qualified Data.Map as M

import ExerciseData ( splitData, toMap, staticValues )

import Text.Parsec
    ( anyChar,
      char,
      string,
      choice,
      manyTill,
      many,
      parse,
      try,
      ParsecT,
      Stream )

placeholder :: Stream s m Char => M.Map String a -> ParsecT s u m String
placeholder m = choice (map string (M.keys m))

replacePlaceholder :: String -> [String] -> M.Map String String -> String 
replacePlaceholder input phs m = T.unpack $ foldl (\x y -> T.replace (T.pack ("#{" ++ y ++ "}")) (T.pack (m M.! y)) x) (T.pack input) phs

pTest :: String -> IO ()
pTest fp = do
    s <- readFile fp
    let (dat, rest) = splitData s
    let uRest = unlines rest
    let pMap = toMap (staticValues dat)
    case parse (many (try (manyTill anyChar (try (string "#{"))) *> (placeholder pMap <* char '}')))  "" uRest of
        Left err  -> error $ show err
        Right x  -> print (replacePlaceholder uRest x pMap)