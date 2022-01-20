plain_parser {
par :: String -> Int -> (String, String)
par str 0 = ("", str)
par [] _ = ("", "")
par ('(':xs) c = let (l, r) = par xs (c+1) in ('(':l, r)
par (')':xs) c = let (l, r) = par xs (c-1) in (')':l, r)
par (x:xs) c = let (l, r) = par xs c in (x:l, r)

parseEither :: String -> Bool -> (String, String, String)
parseEither [] _ = ("", "", "")
parseEither ('(':xs) True = let (a, b) = par xs 1 in let (k, j, z) = parseEither b True in ('(':a++k, j, z)
parseEither ('(':xs) False = let (a, b) = par xs 1 in let (_, j, z) = parseEither b False in ("", '(':a++j, z)
parseEither (' ':xs) True = let (_, j, z) = parseEither xs False in ("", j, z)
parseEither (x:xs) True = let (k, j, z) = parseEither xs True in (x:k, j, z)
parseEither (x:xs) False = if x `elem` " ,)" then ("", "", x:xs) else let ("", j, z) = parseEither xs False in ("", x:j, z)

parseMaybe :: String -> (String, String)
parseMaybe []       = ("", "")
parseMaybe ('(':xs) = let (a, b) = par xs 1 in let (k, j) = parseMaybe b in ('(':a++k, j)
parseMaybe (x:xs)   = if x `elem` " ,)" then ("", x:xs) else let (k, j) = parseMaybe xs in (x:k, j)

parse :: String -> Int -> String
parse [] _ = ""
parse ('M':'a':'y':'b':'e':' ':xs) old = let (a, rest) = parseMaybe xs in if gen == "Nothing" then gen ++ parse rest seed else parse (gen ++ a) seed ++ parse rest seed
   where gen = unGen ( elements ["Just ", "Nothing"] ) (mkQCGen seed) 0
         seed = old + 1
parse ('E':'i':'t':'h':'e':'r':' ':xs) old = let (a, b, rest) = parseEither xs True in if gen == "Left " then parse (gen ++ a) seed ++ parse rest seed else parse (gen ++ b) seed ++ parse rest seed
   where gen = unGen ( elements ["Left ", "Right "] ) (mkQCGen seed) 0
         seed = old + 1
parse ('I':'n':'t':'e':'g':'e':'r':xs) seed = show (unGen ( chooseInt (-99, 99) ) (mkQCGen seed) 0) ++ parse xs (seed + 1)
parse ('B':'o':'o':'l':xs) seed = unGen ( elements ["True", "False"] ) (mkQCGen seed) 0 ++ parse xs (seed + 1)
parse (x:xs) seed = x:parse xs seed
}
sol1 {
#{plain_defaultImports}

#{plain_parser}

sol1 :: IO String
sol1 = return $ parse "#{gen_value1}" #{seed}
}
sol2 {
#{plain_defaultImports}

#{plain_parser}

sol2 :: IO String
sol2 = return $ parse "#{gen_value2}" #{seed}
}
sol3 {
#{plain_defaultImports}

#{plain_parser}

sol3 :: IO String
sol3 = return $ parse "#{gen_value3}" #{seed}
}
sol4 {
#{plain_defaultImports}

#{plain_parser}

sol4 :: IO String
sol4 = return $ parse "#{gen_value4}" #{seed}
}
sol5 {
#{plain_defaultImports}

#{plain_parser}

sol5 :: IO String
sol5 = return $ parse "#{gen_value5}" #{seed}
}
sol6 {
#{plain_defaultImports}

#{plain_parser}

sol6 :: IO String
sol6 = return $ parse "#{gen_value6}" #{seed}
}
sol7 {
#{plain_defaultImports}

#{plain_parser}

sol7 :: IO String
sol7 = return $ parse "#{gen_value7}" #{seed}
}
-----
module Main where
import Test.HUnit
import Data.List (nub)

{- Read up on https://hoogle.haskell.org about the type constructor
 - Either.
 -
 - Then give concrete, different, finite values of the following
 - types. Use every data constructor of Maybe and Either at least
 - once.
 -}

value1 :: #{gen_value1}
value1 = #{sol1}

value2 :: #{gen_value2}
value2 = #{sol2}

value3 :: #{gen_value3}
value3 = #{sol3}

value4 :: #{gen_value4}
value4 = #{sol4}

value5 :: #{gen_value5}
value5 = #{sol5}

value6 :: #{gen_value6}
value6 = #{sol6}

value7 :: #{gen_value7}
value7 = #{sol7}

-- A very simple test suite:
main :: IO ()
main = do _ <- runTestTT $ "value1 and value2 are different" ~:
            (value1 /= value2) @?= True
          _ <- runTestTT $ "value3, value4, value5 are pairwise different" ~:
            (nub [value3, value4, value5] == [value3, value4, value5]) @?= True
          _ <- runTestTT $ "value6 and value7 are different" ~:
            (value6 /= value7) @?= True
          return ()
