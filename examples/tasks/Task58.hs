configGhcErrors:
- deprecation
- empty-enumerations
- identities
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
- unused-local-binds
- unused-matches
- unused-pattern-binds
configHlintErrors:
- Apply De Morgan law
- Avoid lambda
- Avoid reverse
- Collapse lambdas
- Eta reduce
- Evaluate
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant if
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use &&
- Use ++
- Use /=
- Use 1
- "Use :"
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use all
- Use and
- Use any
- Use camelCase
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use drop
- Use elem
- Use even
- Use find
- Use floor
- Use foldr
- Use fromMaybe
- Use fst
- Use guards
- Use head
- Use id
- Use if
- Use infix
- Use init
- Use last
- Use left fold instead of right fold
- Use lefts
- Use list comprehension
- Use list literal pattern
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
- Use otherwise
- Use product
- Use repeat
- Use replicate
- Use right fold instead of left fold
- Use rights
- Use snd
- Use splitAt
- Use sqrt
- Use sum
- Use take
- Use tuple-section
# - Use uncurry
- Use ||
- Used otherwise as a pattern
- Using all on tuple
- Using and on tuple
- Using any on tuple
- Using concat on tuple
- Using elem on tuple
- Using foldr on tuple
- Using length on tuple
- Using maximum on tuple
- Using minimum on tuple
- Using null on tuple
- Using or on tuple
- Using product on tuple
- Using sum on tuple
allowAdding: true
allowModifying: false
allowRemoving: false
configHlintGroups:
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings: []
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Avoid lambda using `infix`
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
- Use foldl
- Use section
- Use tail
configLanguageExtensions:
- NoTemplateHaskell
- TupleSections
# configLanguageExtensions - this sets LanguageExtensions for hlint as well
# configHlintSuggestions   - hlint hints to provide
# configHlintErrors        - hlint hints to enforce
# configGhcWarnings        - GHC warnings to provide as hints
# configGhcErrors          - GHC warnings to enforce
----------
module Main where
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
import qualified Main
import TestHelper (qcWithTimeout, qcWithTimeoutAndRuns)
import Test.QuickCheck
import Test.HUnit ((~:), (@?=), Test)

test :: [ Test ]
test = [ " no identical elements occurring next to each other in output?" ~:
                qcWithTimeout 5000 $ forAll genComp $ propCompress1 . Main.compress
       , " correct elements in correct order in the output?" ~:
                qcWithTimeoutAndRuns 5000 500 $ forAll genComp propCompress2
       , " compress []" ~:
            Main.compress [] @?= []
       , " compress [1]" ~:
            Main.compress [1] @?= [1]
       , " compress [1,2]" ~:
            Main.compress [1,2] @?= [1,2]
       , " compress [2,1]" ~:
            Main.compress [2,1] @?= [2,1]
       , " compress [1,1]" ~:
            Main.compress [1,1] @?= [1]
       ]

propCompress1 :: [Integer] -> Bool
propCompress1 ys | length ys < 2 = True
                 | otherwise = (head ys /= head (tail ys)) && propCompress1 (tail ys)

propCompress2 :: [Integer] -> Bool
propCompress2 xs | length xs < 2 = Main.compress xs == xs
                 | otherwise = let step x y = \ys -> (not . null $ takeWhile (==x) ys)
                                                      && y (dropWhile (==x) ys)
                               in foldr step null (Main.compress xs) xs

genComp :: Gen [Integer]
genComp = do m <- growingElements [1..6]
             n <- arbitrary
             fmap (map (\x -> n + x `mod` m)) arbitrary
