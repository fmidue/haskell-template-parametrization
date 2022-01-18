module DataBuilder (asString, generate, prettify) where
import Data.List.Extra (splitOn, isInfixOf, isPrefixOf)
import Test.QuickCheck.Gen (Gen(unGen), elements)
import Test.QuickCheck.Random (mkQCGen)
import qualified Data.Map as M

allowedChars :: [String]
allowedChars = ["(", ")", ",", "[", "]", " "]

generate :: String -> Int -> Bool -> Int -> [(String, String)] -> String
generate str len err seed m = prettify $ generateData str len len err (toDigits seed) (M.fromList m)

generateData :: String -> Int -> Int -> Bool -> [Int] -> M.Map String String -> String
generateData _ _ _ _ [] _ = ""
generateData [] _ _ _ _ _ = ""
generateData str len og err (z:zs) m | len == 0  = gen filtered ++ generateData xs 0 og err (zs ++ [z]) m
                                     | otherwise = generateData (gen dat) (len - 1) og err (zs ++ [z]) m ++ generateData xs len og err (zs ++ [z]) m
                                      where dat = if "|" `isInfixOf` d x  && (len /= og) then map (\c -> '(':c ++ ")") spliced else spliced
                                            (x, xs) = if any (`isPrefixOf` str) allowedChars then (getPrefix str allowedChars, tail str) else break (\c -> [c] `elem` allowedChars) str
                                            spliced = splitOn "|" (d x)
                                            filtered = filter (not . (\c -> any (`isInfixOf` c) [j++k++l | j<-allowedChars, k<-M.keys m, l<-allowedChars])) dat
                                            gen lst = unGen ( elements lst ) (mkQCGen (fromDigits (z:zs))) 0
                                            fromDigits = foldl addDigit 0
                                               where addDigit num di = 10*num + di
                                            d y = if M.member y m then m M.! y else y

getPrefix :: String -> [String] -> String
getPrefix _ [] = ""
getPrefix str (x:xs) = if x `isPrefixOf` str then x else getPrefix str xs

{-
genDat :: String -> Int -> String 
genDat "Int" seed = int_gen
genDat "Integer" seed = int_gen
genDat "String" seed = string_gen
genDat "[String]" seed = stringList_gen
genDat "[Int]" seed = intList_gen
genDat "[Integer]" seed = intList_gen
genDat str _ = str
-}

toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

prettify :: String -> String 
prettify [] = ""
prettify (' ':' ':str) = prettify (' ':str)
prettify ('(':' ':str) = prettify ('(':str)
prettify (' ':',':str) = prettify (',':str)
prettify (' ':')':str) = prettify (')':str)
prettify (x:str) = x:prettify str

asString :: String
asString = "import Data.List.Extra (splitOn, isInfixOf, isPrefixOf)\nimport Test.QuickCheck.Gen (Gen(unGen), elements)\nimport Test.QuickCheck.Random (mkQCGen)\nimport qualified Data.Map as M\n\nallowedChars :: [String]\nallowedChars = [\"(\", \")\", \",\", \"[\", \"]\", \" \"]\n\ngenerate :: String -> Int -> Bool -> Int -> [(String, String)] -> String\ngenerate str len err seed m = prettify $ generateData str len len err (toDigits seed) (M.fromList m)\n\ngenerateData :: String -> Int -> Int -> Bool -> [Int] -> M.Map String String -> String\ngenerateData _ _ _ _ [] _ = \"\"\ngenerateData [] _ _ _ _ _ = \"\"\ngenerateData str len og err (z:zs) m | len == 0  = gen filtered ++ generateData xs 0 og err (zs ++ [z]) m\n                                     | otherwise = generateData (gen dat) (len - 1) og err (zs ++ [z]) m ++ generateData xs len og err (zs ++ [z]) m\n                                      where dat = if \"|\" `isInfixOf` d x  && (len /= og) then map (\\c -> \'(\':c ++ \")\") spliced else spliced\n                                            (x, xs) = if any (`isPrefixOf` str) allowedChars then (getPrefix str allowedChars, tail str) else break (\\c -> [c] `elem` allowedChars) str\n                                            spliced = splitOn \"|\" (d x)\n                                            filtered = filter (not . (\\c -> any (`isInfixOf` c) [j++k++l | j<-allowedChars, k<-M.keys m, l<-allowedChars])) dat\n                                            gen lst = unGen ( elements lst ) (mkQCGen (fromDigits (z:zs))) 0\n                                            fromDigits = foldl addDigit 0\n                                               where addDigit num di = 10*num + di\n                                            d y = if M.member y m then m M.! y else y\n\ngetPrefix :: String -> [String] -> String\ngetPrefix _ [] = \"\"\ngetPrefix str (x:xs) = if x `isPrefixOf` str then x else getPrefix str xs\n\n{-\ngenDat :: String -> Int -> String \ngenDat \"Int\" seed = int_gen\ngenDat \"Integer\" seed = int_gen\ngenDat \"String\" seed = string_gen\ngenDat \"[String]\" seed = stringList_gen\ngenDat \"[Int]\" seed = intList_gen\ngenDat \"[Integer]\" seed = intList_gen\ngenDat str _ = str\n-}\n\ntoDigits :: Integral a => a -> [a]\ntoDigits 0 = []\ntoDigits x = toDigits (x `div` 10) ++ [x `mod` 10]\n\nprettify :: String -> String \nprettify [] = \"\"\nprettify (\' \':\' \':str) = prettify (\' \':str)\nprettify (\'(\':\' \':str) = prettify (\'(\':str)\nprettify (\' \':\',\':str) = prettify (\',\':str)\nprettify (\' \':\')\':str) = prettify (\')\':str)\nprettify (x:str) = x:prettify str\n"