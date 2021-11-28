{-# LANGUAGE TemplateHaskell #-}
module Task (testParser, combine, task, Task) where

import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter) )
import Language.Haskell.TH (Exp, Q, Loc (loc_filename), location)
import Text.ParserCombinators.Parsec (manyTill, anyChar, try, string, parseTest, many, (<|>), parse, char, newline, many1, lookAhead, notFollowedBy, space, skipMany, noneOf)
import Language.Haskell.TH.Syntax (Lift (lift))
import Text.Parsec.String (Parser)
import Data.List (find)
import Inter (interFile)

data Section = Code [Part] | Data [(String, String)] deriving (Eq, Show)

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

placeholder :: Parser Part
placeholder = string "#{" *> fmap Placeholder (manyTill anyChar (char '}'))

rest :: Parser Part
rest = fmap Rest (try (manyTill anyChar (try $ lookAhead (string "#{")))
                    <|> try (manyTill anyChar (lookAhead seperator))
                    <|> many1 anyChar)

part :: Parser Part
part = try placeholder <|> rest

codeSection :: Parser Section
codeSection = do
    txt <- try (manyTill part seperator) <|> many1 part
    return $ Code txt

placeholderDefinitions :: Parser (String, String)
placeholderDefinitions = try placeholderDefinition <|> multilinePlaceholderDefinition

placeholderDefinition :: Parser (String, String)
placeholderDefinition = do
    n <- skipMany newline >> manyTill (noneOf "\n") (char '=')
    code <- manyTill anyChar (try $ lookAhead (newline >> notFollowedBy space))
    let name = filter (/=' ') n
    return (name, "module Snippet (" ++ name ++ ") where\n" ++ name ++ ":: IO String\n" ++ name ++ " = " ++ code)

multilinePlaceholderDefinition :: Parser (String, String)
multilinePlaceholderDefinition = do
    name <- skipMany newline >> manyTill anyChar (char '{')
    d <- manyTill anyChar (try $ newline >> char '}')
    return (filter (/=' ') name, d)

dataSection :: Parser Section
dataSection = do
    d <- manyTill placeholderDefinitions (try seperator)
    return $ Data d

seperator :: Parser String
seperator = string "\n---" >> manyTill anyChar newline

exercise :: Parser Task
exercise = do
    dat <- dataSection
    rs <- many codeSection
    return $ Task (dat:rs)

combine :: Task -> Task -> IO String
combine _ (Task []) = return ""
combine t (Task (x:xs)) = do
    a <- com t x
    b <- combine t (Task xs)
    return (a++b)

com :: Task -> Section -> IO String
com _ (Data _) = return ""
com _ (Code []) = return ""
com t (Code [x]) = comm t x
com t (Code (x:xs)) = do
    a <- comm t x 
    b <- com t (Code xs)
    return (a++b)

comm :: Task -> Part -> IO String
comm _ (Rest x) = return x
comm t (Placeholder x) = getDataFromTask x t

getDataFromTask :: String -> Task -> IO String
getDataFromTask _ (Task []) = return "-- Placeholder not defined --"
getDataFromTask ph (Task (x:xs)) = case x of 
    Data d -> case find (\(name, _) -> ph == name) d of 
                                  Just (n, y) -> interFile n y
                                  Nothing -> return "-- Placeholder not defined --"
    Code _ -> getDataFromTask ph (Task xs)
                                    

testParser :: IO ()
testParser = parseTest exercise "hallo = sdfgsdfgfdg\ntest {\nwefwefwef\nsdfdfsfsf\n}\ntest2 {\nwefwefwef\nsdfdfsfsf\n}sdf = sdfsd\n---\ndsfgsdsdf #{dgddg} sdfg \n---\ndfgdf dfgdf \n--- df\ngdfg dfgdfgd\n--- df\n"