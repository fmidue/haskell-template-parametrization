{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Task (combineToString, task, loadTask, exercise, parseTask, addSimpleVar, containsVar, Task) where

import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter), quoteFile )
import Language.Haskell.TH (Exp, Q, Loc (loc_filename), location)
import Text.ParserCombinators.Parsec (manyTill, anyChar, try, string, many, (<|>), parse, char, newline, many1, lookAhead, skipMany, noneOf, skipMany1)
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
placeholderDefinitions = try placeholderDefinition <|> try multilinePlaceholderDefinition

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

defaultSection :: Parser Section
defaultSection = do
    d <- many (placeholderDefinitions <* skipMany (string "\r"))
    return $ Data d

seperator :: Parser String
seperator = skipMany1 newline >> string "---" >> manyTill anyChar newline

exercise :: Parser Task
exercise = do
    dat <- try dataSection <|> return (Data [])
    if dat == Data [] then
        do
            d <- defaultSection
            rs <- many codeSection
            return $ Task (d:rs)
    else
        do
            rs <- many codeSection
            return $ Task (dat:rs)


combineToString :: Task -> Bool -> M.Map String String -> IO (String, M.Map String String)
combineToString t isDefaults m = do
    let t' = if isDefaults then withAllVars t else t
    combine t' t' M.empty m

withAllVars :: Task -> Task
withAllVars (Task sections) = Task (sections ++ [Code (map Placeholder (concatMap traverseSection sections))])

traverseSection :: Section -> [String]
traverseSection (Code _) = []
traverseSection (Data []) = []
traverseSection (Data ((name, _):xs)) = name:traverseSection (Data xs)

combine :: Task -> Task -> M.Map String String -> M.Map String String -> IO (String, M.Map String String)
combine _ (Task []) m _ = return ("", m)
combine t (Task (x:xs)) m ma = do
    (a, m') <- com t m ma x
    (b, m'') <- combine t (Task xs) m' ma
    return (a++b, m'')

com :: Task -> M.Map String String -> M.Map String String -> Section -> IO (String, M.Map String String)
com _ m _ (Data _) = return ("", m)
com _ m _ (Code []) = return ("", m)
com t m ma (Code [x]) = do
    (a, m') <- comm t m ma x
    return (a, m')
com t m ma (Code (x:xs)) = do
    (a, m') <- comm t m ma x
    (b, m'') <- com t m' ma (Code xs)
    return (a++b, m'')

comm :: Task -> M.Map String String -> M.Map String String -> Part -> IO (String, M.Map String String)
comm _ m _ (Rest x) = return (x, m)
comm t m ma (Placeholder x) = if x `elem` M.keys m then return (m M.! x, m) else do
    (dat, m') <- getDataFromTask x t m ma
    let m'' = M.insert x dat m'
    return (dat, m'')

getDataFromTask :: String -> Task -> M.Map String String -> M.Map String String -> IO (String, M.Map String String)
getDataFromTask ph (Task []) m ma = if ph `elem` M.keys ma then return (ma M.! ph, m) else return ("-- Placeholder not defined --", m)
getDataFromTask ph t@(Task (x:xs)) m ma = case x of
    Data d -> case find (\(name, _) -> ph == name) d of
                                  Just (n, y) -> do
                                      (content, m') <- concatIO $ map (comm t m ma) y
                                      inter <- interFile n content
                                      return (inter, m')
                                  Nothing -> getDataFromTask ph (Task xs) m ma
    Code _ -> getDataFromTask ph (Task xs) m ma

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

concatIO :: [IO (String, M.Map String String)] -> IO (String, M.Map String String)
concatIO [] = return ("", M.empty )
concatIO (x:xs) = do
    (content, m) <- x
    (y, m') <- concatIO xs
    return (content ++ y, M.union m m')
