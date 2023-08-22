enableWhitespaceWatermarking = return "True"
moduleName = return "Solution"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use ++
- Use 1
- "Use :"
- Use all
- Use and
- Use any
- Use camelCase
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use even
- Use find
- Use floor
- Use foldr
- Use fromMaybe
- Use guards
- Use if
- Use infix
- Use lefts
- Use list comprehension
- Use map
- Use map once
- Use mapMaybe
- Use maximum
# - Use maybe
- Use minimum
- Use negate
- Use newtype instead of data
- Use notElem
- Use odd
- Use or
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tuple-section
# - Use uncurry
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings: []

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Use foldl
- Use tail

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Prelude hiding ((!!), head, tail, last, init, take, drop, splitAt, dropWhile, zip, zipWith)
import Test.QuickCheck

{- Implement, via pattern-matching, a function which combines repeated
 - values occurring next to each other, into single entries. For
 - example:
 -
 -   compress [1,3,3,3,2,4,4,2,4] = [1,3,2,4,2,4]
 -}

compress :: [Integer] -> [Integer]
compress = undefined

main :: IO ()
main = do putStrLn "executing first test set:"
          quickCheck $ \n -> compress [1..n] == [1..n]
          putStrLn "executing second test set:"
          quickCheck $ \xs -> length (compress xs) <= length xs
          putStrLn "executing third test set:"
          quickCheck $ \xs y zs -> compress (xs ++ [y,y] ++ zs) == compress (xs ++ [y] ++ zs)
-------------------------------------
module Test (test) where
import Prelude
import qualified #{moduleName}
import TestHelper (qcWithTimeout, qcWithTimeoutAndRuns)
import Test.QuickCheck
import Test.HUnit ((~:), (@?=), Test)

test :: [ Test ]
test = [ " no identical elements occurring next to each other in output?" ~:
                qcWithTimeout 500000 $ forAll genComp $ propCompress1 . #{moduleName}.compress
       , " correct elements in correct order in the output?" ~:
                qcWithTimeoutAndRuns 500000 500 $ forAll genComp propCompress2
       , " compress []" ~:
            #{moduleName}.compress [] @?= []
       , " compress [1]" ~:
            #{moduleName}.compress [1] @?= [1]
       , " compress [1,2]" ~:
            #{moduleName}.compress [1,2] @?= [1,2]
       , " compress [2,1]" ~:
            #{moduleName}.compress [2,1] @?= [2,1]
       , " compress [1,1]" ~:
            #{moduleName}.compress [1,1] @?= [1]
       ]

propCompress1 :: [Integer] -> Bool
propCompress1 ys | length ys < 2 = True
                 | otherwise = (head ys /= head (tail ys)) && propCompress1 (tail ys)

propCompress2 :: [Integer] -> Bool
propCompress2 xs | length xs < 2 = #{moduleName}.compress xs == xs
                 | otherwise = let step x y = \ys -> (not . null $ takeWhile (==x) ys)
                                                      && y (dropWhile (==x) ys)
                               in foldr step null (#{moduleName}.compress xs) xs

genComp :: Gen [Integer]
genComp = do m <- growingElements [1..6]
             n <- arbitrary
             fmap (map (\x -> n + x `mod` m)) arbitrary
