seed = return "Wintersemester"
watermark {
import Test.QuickCheck.Gen
import Test.QuickCheck.Random (mkQCGen)

watermark :: IO String
watermark = return $ unGen ( elements ["course", "lecture"] ) (mkQCGen #{seed}) 0
}
plain_dataBuilder {
import Data.List.Extra (splitOn, isInfixOf, isPrefixOf, (\\))
import Test.QuickCheck.Gen (Gen(unGen), elements)
import Test.QuickCheck.Random (mkQCGen)
import qualified Data.Map as M
import Test.QuickCheck (chooseInt)

allowedChars :: [String]
allowedChars = ["(", ")", ",", "[", "]", " "]

generateData :: String -> Int -> Int -> Int -> [(String, String)] -> String
generateData str minLength maxLength seed m =  prettify $ removeEndSpaces (removeEndSpaces (generateData' str minLength maxLength 0 (toDigits seed) (M.fromList m)))
   where removeEndSpaces x = dropWhile (' '==) (reverse x)

generateData' :: String -> Int -> Int -> Int -> [Int] -> M.Map String String -> String
generateData' _ _ _ _ [] _ = ""
generateData' [] _ _ _ _ _ = ""
generateData' str minLength maxLength counter (z:zs) m | maxLength == 0 || maxLength <= counter = (if null leaf then generateData' (gen dat) minLength maxLength (counter + 1) (zs ++ [z]) m else gen leaf) ++ generateData' xs 0 0 counter (zs ++ [z]) m
                                                          | otherwise = generateData' (if counter < minLength && not (null branch) then gen branch else gen dat) minLength maxLength (counter + 1) (zs ++ [z]) m ++ generateData' xs minLength maxLength counter (zs ++ [z]) m
                                                          where dat = if "|" `isInfixOf` d  && counter /= 0 then map (\c -> if isKey then '(':c ++ ")" else c) spliced else spliced
                                                                (x, xs) = if any (`isPrefixOf` str) allowedChars then (getPrefix str allowedChars, tail str) else break (\c -> [c] `elem` allowedChars) str
                                                                spliced = splitOn "|" d
                                                                branch = dat \\ leaf
                                                                allowed = [j++k++l | j<-allowedChars, k<-M.keys m, l<-allowedChars]
                                                                leaf = filter (not . (\c -> any (`isInfixOf` c) allowed)) dat
                                                                gen lst = unGen ( elements lst ) (mkQCGen (fromDigits (z:zs))) 0
                                                                fromDigits = foldl addDigit 0
                                                                   where addDigit num di = 10*num + di
                                                                (isKey, d) = if M.member x m then (True, m M.! x) else (False, x)

getPrefix :: String -> [String] -> String
getPrefix _ [] = ""
getPrefix str (x:xs) = if x `isPrefixOf` str then x else getPrefix str xs

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
}
