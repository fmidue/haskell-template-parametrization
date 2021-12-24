{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Task (testParser, combineToString, task, loadTask, exercise, parseTask, addSimpleVar, containsVar, Task) where

import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter), quoteFile )
import Language.Haskell.TH (Exp, Q, Loc (loc_filename), location)
import Text.ParserCombinators.Parsec (manyTill, anyChar, try, string, parseTest, many, (<|>), parse, char, newline, many1, lookAhead, skipMany, noneOf, skipMany1)
import Language.Haskell.TH.Syntax (Lift (lift))
import Text.Parsec.String (Parser)
import Data.List (find)
import Inter (interFile)
import qualified Data.Map as M

data Section = Code [Part] | Data [(String, [Part])] deriving (Eq, Show)

data Part = Placeholder String | Rest String deriving (Eq, Show)

newtype Task = Task [Section] deriving (Eq, Show)

task :: QuasiQuoter
task = QuasiQuoter
    taskExpr
    undefined
    undefined
    undefined

instance Lift Section where
    lift (Code c) = [| Code c |]
    lift (Data d) = [| Data d |]

instance Lift Part where
    lift (Placeholder p) = [| Placeholder p |]
    lift (Rest r) = [| Rest r |]


instance Lift Task where
    lift (Task t) = [| Task t |]

taskExpr :: String -> Q Exp
taskExpr str = do
    filename <- fmap loc_filename location
    case parse exercise filename str of
        Left err -> error $ show err
        Right ex -> [| ex |]

loadTask :: QuasiQuoter
loadTask = quoteFile task

parseTask :: String -> IO Task
parseTask str = do
    case parse exercise "" str of
        Left err -> error $ show err
        Right ex -> return ex

placeholder :: Parser Part
placeholder = string "#{" *> fmap Placeholder (manyTill anyChar (char '}'))

rest :: Parser Part
rest = fmap Rest (try (manyTill anyChar (try $ lookAhead (string "#{")))
                    <|> try (manyTill anyChar (lookAhead seperator))     --Unsafe
                    <|> many1 anyChar)

singlelineRest :: Parser Part
singlelineRest = fmap Rest(try (manyTill anyChar (try $ lookAhead (string "#{" <|> string "\n"))))

multilineRest :: Parser Part
multilineRest = fmap Rest(try (manyTill anyChar (try $ lookAhead (string "#{" <|> string "\n}"))))

part :: Parser Part
part = try placeholder <|> rest

singlelinePart :: Parser Part
singlelinePart = try placeholder <|> singlelineRest

multilinePart :: Parser Part
multilinePart = try placeholder <|> multilineRest

codeSection :: Parser Section
codeSection = do
    txt <- try (manyTill part seperator) <|> many1 part
    return $ Code txt

placeholderDefinitions :: Parser (String, [Part])
placeholderDefinitions = try placeholderDefinition <|> multilinePlaceholderDefinition

placeholderDefinition :: Parser (String, [Part])
placeholderDefinition = do
    n <- skipMany newline >> manyTill (noneOf "\n") (char '=')
    code <- manyTill singlelinePart (lookAhead newline)
    let name = filter (/=' ') n
    return (name, Rest ("module Snippet (" ++ name ++ ") where\n" ++ name ++ " :: IO String\n" ++ name ++ " ="):code)

multilinePlaceholderDefinition :: Parser (String, [Part])
multilinePlaceholderDefinition = do
    name <- skipMany newline >> manyTill anyChar (char '{')
    d <- manyTill multilinePart (try $ newline >> char '}')
    return (filter (/=' ') name, d)

dataSection :: Parser Section
dataSection = do
    d <- manyTill (placeholderDefinitions <* skipMany (string "\r")) (try seperator)
    return $ Data d

seperator :: Parser String
seperator = skipMany1 newline >> string "---" >> manyTill anyChar newline

exercise :: Parser Task
exercise = do
    dat <- try dataSection <|> return (Data [])
    rs <- many codeSection
    return $ Task (dat:rs)

combineToString :: Task -> M.Map String String -> IO (String, M.Map String String)
combineToString t = combine t t

combine :: Task -> Task -> M.Map String String -> IO (String, M.Map String String)
combine _ (Task []) m = return ("", m)
combine t (Task (x:xs)) m = do
    (a, m') <- com t m x
    (b, m'') <- combine t (Task xs) m'
    return (a++b, m'')

com :: Task -> M.Map String String -> Section -> IO (String, M.Map String String)
com _ m (Data _) = return ("", m)
com _ m (Code []) = return ("", m)
com t m (Code [x]) = do
    (a, m') <- comm t m x
    return (a, m')
com t m (Code (x:xs)) = do
    (a, m') <- comm t m x
    (b, m'') <- com t m' (Code xs)
    return (a++b, m'')

comm :: Task -> M.Map String String -> Part -> IO (String, M.Map String String)
comm _ m (Rest x) = return (x, m)
comm t m (Placeholder x) = if x `elem` M.keys m then return (m M.! x, m) else do
    dat <- getDataFromTask x t m
    let m' = M.insert x dat m
    return (dat, m')

getDataFromTask :: String -> Task -> M.Map String String -> IO String
getDataFromTask _ (Task []) _ = return "-- Placeholder not defined --"
getDataFromTask ph t@(Task (x:xs)) m = case x of
    Data d -> case find (\(name, _) -> ph == name) d of
                                  Just (n, y) -> do
                                      content <- concatIO $ map (comm t m) y
                                      interFile n content
                                  Nothing -> getDataFromTask ph (Task xs) m
    Code _ -> getDataFromTask ph (Task xs) m

containsVar :: String -> Task -> Bool
containsVar ph (Task sections) = any f sections
   where f x = case x of
          Data d -> case find (\(name, _) -> ph == name) d of
            Just _ -> True
            Nothing -> False
          Code _ -> False

addVar :: (String, [Part]) -> Task -> Task
addVar (name, content) (Task sections) = Task (Data [(name, Rest ("module Snippet (" ++ name ++ ") where\n" ++ name ++ " :: IO String\n" ++ name ++ " = return \"") : content ++ [Rest "\""])] : sections)

addRawVar :: (String, [Part]) -> Task -> Task
addRawVar (name, content) (Task sections) = Task (Data [(name, content)] : sections)

addSimpleVar :: (String, String) -> Task -> Task
addSimpleVar (name, content) (Task sections) = Task (Data [(name, [Rest (("module Snippet (" ++ name ++ ") where\n" ++ name ++ " :: IO String\n" ++ name ++ " = return \"") ++ content ++ "\"")])] : sections)

addSimpleRawVar :: (String, String) -> Task -> Task
addSimpleRawVar (name, content) (Task sections) = Task (Data [(name, [Rest content])] : sections)

concatIO :: [IO (String, M.Map String String)] -> IO String
concatIO [] = return ""
concatIO (x:xs) = do
    (content, _) <- x
    y <- concatIO xs
    return (content ++ y)

testParser :: IO ()
testParser = parseTest exercise "task = return \"23\"\r\ntheModulus {\r\nmodule Snippet (theModulus) where\n\nimport Test.QuickCheck.Gen\n\ntheModulus = do \n    x <- generate $ choose (5, 20) `suchThat` is_prime\n    return (show x)\n\nis_prime :: Int -> Bool\nis_prime 1 = False\nis_prime 2 = True\nis_prime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False\n           | otherwise = True\r\n}\r\n-------\nmodule Main where\n\nimport Test.QuickCheck\n\n{- Recall the stuff from lec"