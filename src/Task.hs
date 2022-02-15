{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

{-|
Module      : Task

Contains all important functions to parse, modify and translate a task.
-}

module Task 
    ( -- * Types
    Task,
    Part(Rest, Placeholder),
    -- * Task manipulation
    addSimpleVar, addRawVar, addVar, containsVar,
    -- * Parser and combinator
    combineToString, task, exercise, parseTask) where

import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter) )
import Language.Haskell.TH (Exp, Q, Loc (loc_filename), location)
import Text.ParserCombinators.Parsec (manyTill, anyChar, try, string, many, (<|>), parse, char, newline, many1, lookAhead, skipMany, noneOf)
import Language.Haskell.TH.Syntax (Lift (lift))
import Text.Parsec.String (Parser)
import Data.List (find, isPrefixOf, isInfixOf, intersperse)
import Inter (interFile)
import qualified Data.Map as M
import Seed (stringToSeed, seedToString)
import Data.List.Extra (splitOn)

-- | Can either contain a section with text and placeholders
-- or a section with all placeholder definitions
data Section = Code [Part] | Data [(String, [Part])] deriving (Eq, Show)

-- | Can either be a placeholder or a simple string
data Part = Placeholder String -- ^ name of the placeholder
          | Rest String        -- ^ simple string. Will not be changed.
          deriving (Eq, Show)

-- | Contains all task information 
newtype Task = Task [Section] deriving (Eq, Show)

-- | A QuasiQuoter to translate a task
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

-- | Used to translate to an expression
taskExpr :: String -> Q Exp
taskExpr str = do
    filename <- fmap loc_filename location
    case parse exercise filename str of
        Left err -> error $ show err
        Right ex -> [| ex |]

-- | Used to parse tasks from a string
parseTask :: String -> IO Task
parseTask str =
    case parse exercise "" str of
        Left err -> error $ show err
        Right ex -> return ex

-- | Parses the name of a placeholder
placeholder :: Parser Part
placeholder = string "#{" *> fmap Placeholder (manyTill anyChar (char '}'))

-- | Reads string until placeholder, end of section or end of file
rest :: Parser Part
rest = fmap Rest (try (manyTill anyChar (try $ lookAhead (string "#{" <|> (separator >> return ""))))
                    <|> try (manyTill anyChar (lookAhead separator))
                    <|> many1 anyChar)

-- | Reads string inside a placeholder definition until placeholder or end of line
singlelineRest :: Parser Part
singlelineRest = fmap Rest(try (manyTill anyChar (try $ lookAhead (string "#{" <|> string "\n"))))

-- | Reads string inside a placeholder definition until placeholder or end placeholder definition
multilineRest :: Parser Part
multilineRest = fmap Rest(try (manyTill anyChar (try $ lookAhead (string "#{" <|> string "\n}"))))

-- | Reads either a placeholder or a simple string
part :: Parser Part
part = try placeholder <|> rest

-- | Reads either a placeholder or a simple string inside a single line placeholder
singlelinePart :: Parser Part
singlelinePart = try placeholder <|> singlelineRest

-- | Reads either a placeholder or a simple string inside a multi line placeholder
multilinePart :: Parser Part
multilinePart = try placeholder <|> multilineRest

codeSection :: Parser Section
codeSection = do
    txt <- try ((++) <$> manyTill part (try $ lookAhead separator) <*> fmap (: []) separator) <|> many1 part
    return $ Code txt

placeholderDefinitions :: Parser (String, [Part])
placeholderDefinitions = try placeholderDefinition <|> try multilinePlaceholderDefinition

placeholderDefinition :: Parser (String, [Part])
placeholderDefinition = do
    n <- skipMany newline >> manyTill (noneOf "\n") (char '=')
    code <- manyTill singlelinePart (lookAhead newline)
    let name = filter (/=' ') n
    return $ modifyVarPre name code

multilinePlaceholderDefinition :: Parser (String, [Part])
multilinePlaceholderDefinition = do
    name <- skipMany newline >> manyTill anyChar (char '{')
    code <- manyTill multilinePart (try $ newline >> char '}')
    return (filter (/=' ') name, if "plain_" `isPrefixOf` name then code else Rest ("module Snippet (" ++ name ++ ") where\n"):Placeholder "plain_defaultImports":Rest "\n":code)

dataSection :: Parser Section
dataSection = do
    d <- manyTill (placeholderDefinitions <* skipMany (string "\r")) (try separator)
    return $ Data d

defaultSection :: Parser Section
defaultSection = do
    d <- many (placeholderDefinitions <* skipMany (string "\r"))
    return $ Data d

separator :: Parser Part
separator = do
    nl <- many1 newline 
    sep <- string "---" 
    name <- manyTill anyChar newline
    return $ Rest (nl ++ sep ++ name ++ "\n")

-- | A parser to be used to parse the tasks
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

-- | Translates a given task to the ouput string and map with all placeholders
combineToString :: Task                -- ^ the task
                -> Bool                -- ^ is task the defaults file?
                -> M.Map String String -- ^ default placeholders and placeholders from previous task if the given task is a solution
                -> IO (String, M.Map String String)
combineToString t isDefaults m = do
    let t' = if isDefaults then withAllVars t else withDefaultVars t
    (output, taskMap) <- combine t' t' M.empty m
    return (unlines (reverse (removeDefaults (reverse (lines output)))), taskMap)

removeDefaults :: [String] -> [String]
removeDefaults [] = ["Error."]
removeDefaults (x:xs) = if x == "{-" then xs else removeDefaults xs

defaultNames :: [String]
defaultNames = ["seed", "enableWhitespaceWatermarking", "plain_defaultFunctions", "plain_defaultImports"]

withAllVars :: Task -> Task
withAllVars (Task sections) = Task (sections ++ [Code (Rest "\n{-\n" :map Placeholder (concatMap traverseSection sections) ++ [Rest "\n-}\n"])])

withDefaultVars :: Task -> Task
withDefaultVars (Task sections) = Task (sections ++ [Code (Rest "\n{-\n" : map Placeholder defaultNames ++ [Rest "\n-}\n"])])

traverseSection :: Section -> [String]
traverseSection (Code _) = []
traverseSection (Data []) = []
traverseSection (Data ((name, _):xs)) = name:traverseSection (Data xs)

combine :: Task -> Task -> M.Map String String -> M.Map String String -> IO (String, M.Map String String)
combine _ (Task []) m _ = return ("", m)
combine t (Task (x:xs)) m ma = do
    (a, m') <- com t m ma x
    (b, m'') <- combine t (Task xs) m' ma
    return (a++b, M.union m'' ma)

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
    let modified = modifyVar x dat
    let m'' = M.insert x modified m'
    return (modified, m'')

getDataFromTask :: String -> Task -> M.Map String String -> M.Map String String -> IO (String, M.Map String String)
getDataFromTask ph (Task []) m ma = if ph `elem` M.keys ma then return (ma M.! ph, m) else return ("-- Placeholder '" ++ ph ++ "' not defined --", m)
getDataFromTask ph t@(Task (x:xs)) m ma = case x of
    Data d -> case find (\(name, _) -> ph == name) d of
                                  Just (n, y) -> do
                                      (content, m') <- concatIO $ map (comm t m ma) y
                                      inter <- if "plain_" `isPrefixOf` n then return content else interFile n content
                                      return (inter, m')
                                  Nothing -> getDataFromTask ph (Task xs) m ma
    Code _ -> getDataFromTask ph (Task xs) m ma

-- | Check if a variable is defined in the task
containsVar :: String -- ^ variable name to check
            -> Task   -- ^ task to check
            -> Bool
containsVar ph (Task sections) = any f sections
   where f x = case x of
          Data d -> case find (\(name, _) -> ph == name) d of
            Just _ -> True
            Nothing -> False
          Code _ -> False

-- | Used to add variable to a task
addVar :: (String, [Part]) -- ^ should contain name and content of the var. Can also contain placeholders
       -> Task             -- ^ variable gets added to this task
       -> Task
addVar (name, content) (Task sections) = Task (Data [(name, Rest ("module Snippet (" ++ name ++ ") where\n" ++ name ++ " :: IO String\n" ++ name ++ " = return \"") : content ++ [Rest "\""])] : sections)

-- | Used to add variable to a task but without module definition
addRawVar :: (String, [Part]) -- ^ should contain name and content of the var. Can also contain placeholders
          -> Task             -- ^ variable gets added to this task
          -> Task
addRawVar (name, content) (Task sections) = Task (Data [(name, content)] : sections)

-- | Used to add a variable to a task
addSimpleVar :: (String, String) -- ^ should contain name and content of the var
             -> Task             -- ^ variable gets added to this task
             -> Task
addSimpleVar (name, content) (Task sections) = Task (Data [(name, [Rest (("module Snippet (" ++ name ++ ") where\n" ++ name ++ " :: IO String\n" ++ name ++ " = return \"") ++ content ++ "\"")])] : sections)

modifyVarPre :: String -> [Part] -> (String, [Part])
modifyVarPre name content | "ungen_" `isPrefixOf` name = (name, Rest ("module Snippet (" ++ name ++ ") where\nimport Data.List (isPrefixOf, isSuffixOf)\nimport Test.QuickCheck.Gen\nimport Test.QuickCheck.Random (mkQCGen)\n" ++ name ++ " :: IO String\n" ++ name ++ " = return $ if isPrefixOf \"\\\"\" str && isSuffixOf \"\\\"\" str then init (tail str) else str\n  where str = show (unGen (") : content ++ [Rest " ) (mkQCGen ", Placeholder "seed", Rest ") 0)"])
                          | "plain_" `isPrefixOf` name = (name, content)
                          | ":" `isInfixOf` name = (head sname, Rest ("module Snippet (" ++ normalizedName ++ ") where\n") : Rest "\n":Placeholder "plain_defaultImports" : intersperse (Rest "\n") (map Placeholder (tail sname)) ++ Rest "\n":Placeholder "plain_defaultFunctions":Rest ("\n" ++ normalizedName ++ " :: IO String\n" ++ normalizedName ++ " ="):content)
                          | otherwise = (name, Rest ("module Snippet (" ++ name ++ ") where\n"):Placeholder "plain_defaultImports":Rest "\n":Placeholder "plain_defaultFunctions":Rest ("\n" ++ name ++ " :: IO String\n" ++ name ++ " ="):content)
                          where normalizedName = head sname
                                sname = splitOn ":" name

modifyVar :: String -> String -> String
modifyVar name content | name == "seed" = seedToString $ stringToSeed content
                       | otherwise = content

concatIO :: [IO (String, M.Map String String)] -> IO (String, M.Map String String)
concatIO [] = return ("", M.empty )
concatIO (x:xs) = do
    (content, m) <- x
    (y, m') <- concatIO xs
    return (content ++ y, M.union m m')
