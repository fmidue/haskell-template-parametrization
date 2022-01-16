module DataBuilder (asString) where
import Data.List.Extra (splitOn, isInfixOf, isPrefixOf)
import Test.QuickCheck.Gen (Gen(unGen), elements)
import Test.QuickCheck.Random (mkQCGen)
import qualified Data.Map as M

allowedChars :: [String]
allowedChars = ["(", ")", ",", "[", "]", " "]

generateData :: String -> Int -> Bool -> [Int] -> M.Map String String -> String
generateData _ _ _ [] _ = ""
generateData [] _ _ _ _ = ""
generateData str len err (z:zs) m    | len == 0 = gen filtered ++ generateData xs 0 err (zs ++ [z]) m
                                     | otherwise = generateData (gen dat) (len - 1) err (zs ++ [z]) m ++ generateData xs len err (zs ++ [z]) m
                                      where dat = if "|" `isInfixOf` d x then map (\c -> '(':c ++ ")") spliced else spliced
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

asString :: String 
asString = "import Data.List.Extra (splitOn, isInfixOf, isPrefixOf)\nimport Test.QuickCheck.Gen (Gen(unGen), elements)\nimport Test.QuickCheck.Random (mkQCGen)\nimport qualified Data.Map as M\n\nallowedChars :: [String]\nallowedChars = [\"(\", \")\", \",\", \"[\", \"]\", \" \"]\n\ngenerateData :: String -> Int -> Bool -> [Int] -> M.Map String String -> String\ngenerateData _ _ _ [] _ = \"\"\ngenerateData [] _ _ _ _ = \"\"\ngenerateData str len err (z:zs) m    | len == 0 = gen filtered ++ generateData xs 0 err (zs ++ [z]) m\n                                     | otherwise = generateData (gen dat) (len - 1) err (zs ++ [z]) m ++ generateData xs len err (zs ++ [z]) m\n                                      where dat = if \"|\" `isInfixOf` d x then map (\\c -> \'(\':c ++ \")\") spliced else spliced\n                                            (x, xs) = if any (`isPrefixOf` str) allowedChars then (getPrefix str allowedChars, tail str) else break (\\c -> [c] `elem` allowedChars) str\n                                            spliced = splitOn \"|\" (d x)\n                                            filtered = filter (not . (\\c -> any (`isInfixOf` c) [j++k++l | j<-allowedChars, k<-M.keys m, l<-allowedChars])) dat\n                                            gen lst = unGen ( elements lst ) (mkQCGen (fromDigits (z:zs))) 0\n                                            fromDigits = foldl addDigit 0\n                                               where addDigit num di = 10*num + di\n                                            d y = if M.member y m then m M.! y else y\n\ngetPrefix :: String -> [String] -> String\ngetPrefix _ [] = \"\"\ngetPrefix str (x:xs) = if x `isPrefixOf` str then x else getPrefix str xs\n"
