{-# LANGUAGE TemplateHaskell #-}
module Task (testParser, combineToString, task, loadTask, Task) where

import Language.Haskell.TH.Quote ( QuasiQuoter(QuasiQuoter), quoteFile )
import Language.Haskell.TH (Exp, Q, Loc (loc_filename), location)
import Text.ParserCombinators.Parsec (manyTill, anyChar, try, string, parseTest, many, (<|>), parse, char, newline, many1, lookAhead, notFollowedBy, space, skipMany, noneOf)
import Language.Haskell.TH.Syntax (Lift (lift))
import Text.Parsec.String (Parser)
import Data.List (find)
import Inter (interFile)
import qualified Data.Map as M

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

loadTask :: QuasiQuoter
loadTask = quoteFile task

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
    d <- manyTill (placeholderDefinitions <* skipMany (string "\r")) (try seperator)
    return $ Data d

seperator :: Parser String
seperator = string "\n---" >> manyTill anyChar newline

exercise :: Parser Task
exercise = do
    dat <- dataSection
    rs <- many codeSection
    return $ Task (dat:rs)

combineToString :: Task -> IO String
combineToString t = combine t t M.empty

combine :: Task -> Task -> M.Map String String -> IO String
combine _ (Task []) _ = return ""
combine t (Task (x:xs)) m = do
    (a, m') <- com t m x
    b <- combine t (Task xs) m'
    return (a++b)

com :: Task -> M.Map String String -> Section -> IO (String, M.Map String String)
com _ m (Data _) = return ("", m)
com _ m (Code []) = return ("", m)
com t m (Code [x]) = do
    (a, m') <- comm t m x
    return (a, m')
com t m (Code (x:xs)) = do
    (a, m') <- comm t m x
    (b, _) <- com t m' (Code xs)
    return (a++b, m')

comm :: Task -> M.Map String String -> Part -> IO (String, M.Map String String)
comm _ m (Rest x) = return (x, m)
comm t m (Placeholder x) = if x `elem` M.keys m then return (m M.! x, m) else do
    dat <- getDataFromTask x t
    let m' = M.insert x dat m
    return (dat, m')

getDataFromTask :: String -> Task -> IO String
getDataFromTask _ (Task []) = return "-- Placeholder not defined --"
getDataFromTask ph (Task (x:xs)) = case x of
    Data d -> case find (\(name, _) -> ph == name) d of
                                  Just (n, y) -> interFile n y
                                  Nothing -> return "-- Placeholder not defined --"
    Code _ -> getDataFromTask ph (Task xs)


testParser :: IO ()
testParser = parseTest exercise "task = return \"23\"\r\ntheModulus {\r\nmodule Snippet (theModulus) where\n\nimport Test.QuickCheck.Gen\n\ntheModulus = do \n    x <- generate $ choose (5, 20) `suchThat` is_prime\n    return (show x)\n\nis_prime :: Int -> Bool\nis_prime 1 = False\nis_prime 2 = True\nis_prime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False\n           | otherwise = True\r\n}\r\n-------\nmodule Main where\n\nimport Test.QuickCheck\n\n{- Recall the stuff from lecture slide 123. Also, remember Task #{task} \nfrom last week. Here is a function:\n\n-}\n\noriginal :: [Integer] -> Integer\n\noriginal [] = 7\n\noriginal (x:xs) | (x `mod` #{theModulus}) == 0 = 13 + original xs\n\n| otherwise = x * original xs\n\n-- Reimplement it with foldr:\n\nalternative :: [Integer] -> Integer\n\nalternative = foldr undefined undefined\n\n-- The obvious test suite (but we'll do more elaborate stuff in \nAutotool's feedback as well):\n\nmain :: IO ()\n\nmain = quickCheck $ list -> original list == alternative list\n\n-----------------\n\n# From here on comes hidden stuff that students do not get to see:\n\nconfigGhcErrors:\n\n- deprecation\n\n- empty-enumerations\n\n- identities\n\n- name-shadowing\n\n- overflowed-literals\n\n- overlapping-patterns\n\n- tabs\n\nconfigHlintErrors:\n\n- Avoid reverse\n\n- Collapse lambdas\n\n- Eta reduce\n\n- Evaluate\n\n- Length always non-negative\n\n- Move brackets to avoid $\n\n- Redundant $\n\n- Redundant /=\n\n# ... and many more configuration options, as well as further sections \nas follows, for different purposes:\n\n--------------\n\n{- the hidden testing module -}\n\nmodule Test (test) where\n\nimport qualified Main\n\nimport TestHelper (qcWithTimeoutAndRuns)\n\nimport Test.HUnit ((~:), Test)\n\ntest :: [ Test ]\n\ntest = ...\n\n--------------\n\n{- possibly further modules ... -}"