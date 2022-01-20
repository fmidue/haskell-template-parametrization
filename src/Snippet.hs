{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Snippet (snippet, Snippet (Snippet)) where
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax (Lift (lift))
import Language.Haskell.TH (Exp, Q, Loc (loc_filename), location)
import Text.Parsec ( parse, newline, string, manyTill, anyChar, eof, many, skipMany )
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec (try)

newtype Snippet = Snippet (String, String) deriving Show

snippet :: QuasiQuoter
snippet = QuasiQuoter
    snippetExpr
    undefined
    undefined
    undefined

instance Lift Snippet where
    lift (Snippet t) = [| Snippet t |]


snippetExpr :: String -> Q Exp
snippetExpr str = do
    filename <- fmap loc_filename location
    case parse snipp filename str of
        Left err -> error $ show err
        Right ex -> [| ex |]

singleImport :: Parser String 
singleImport = do 
    i <- try $ string "import "
    rest <- manyTill anyChar newline <* skipMany newline
    return $ i ++ rest

imports :: Parser [String]
imports = try $ many singleImport

snipp :: Parser Snippet
snipp = do
    i <- imports
    code <- manyTill anyChar eof
    return $ Snippet (unlines i, code)