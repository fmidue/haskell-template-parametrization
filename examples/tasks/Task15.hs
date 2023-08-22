enableWhitespaceWatermarking = return "True"
moduleName = return "Task15"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
# - incomplete-patterns # might reveal list patterns
# - incomplete-uni-patterns # might reveal list patterns
- missing-signatures
- name-shadowing
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- unused-local-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Use ++
- Use 1
- Use all
- Use and
- Use any
- Use catMaybes
- Use concat
- Use concatMap
- Use const
# - Use curry
- Use find
- Use floor
- Use foldl
- Use foldr
- Use fromMaybe
- Use infix
- Use lefts
- Use list comprehension
- Use map once
- Use mapMaybe
# - Use maybe
- Use negate
- Use newtype instead of data
- Use notElem
- Use or
- Use repeat
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Prelude hiding (($))

{-
 - This optional task is more for mathematical fun than a serious
 - programming task.
 -
 - Give Haskell defined infinite lists for:
 -}

-- 1. the set of all integer numbers,
ints :: [Integer]
ints = undefined

-- 2. the set of all pairs of natural numbers,
pairs :: [(Integer,Integer)]
pairs = undefined

-- 3. the set of all triples of two natural numbers and an integer number.
triples :: [(Integer,Integer,Integer)]
triples = undefined

{-
 - None of the above lists should contain duplicate or superfluous
 - elements. Also, it is a good idea, before uploading your solution,
 - to convince yourself (locally, in a ghci session) that all the
 - following tests return True:
 -
 -   10 `elem` ints
 -   -10 `elem` ints
 -   (0,10) `elem` pairs
 -   (10,0) `elem` pairs
 -   (10,10) `elem` pairs
 -   (0,10,0) `elem` triples
 -   (0,10,-10) `elem` triples
 -   (10,0,0) `elem` triples
 -   (10,0,10) `elem` triples
 -   (10,10,10) `elem` triples
 -   (10,10,-10) `elem` triples
 -
 - Also locally, you can of course inspect some prefixes of your
 - infinite lists, with calls like:
 -
 -   take 100 pairs
 -
 - The goal is that you convince yourself that your code does not
 - "hang" in the sense of taking too long, infinitely long, to produce
 - even a finite prefix of the list for consumption. Otherwise,
 - Autotool will have no other choice than to time out on your
 - submission.
 -
 - A correct solution of this task will successfully pass all the
 - above tests.
 -
 - As an exception from standard practice, the use of !! is not
 - discouraged in this task. Actually, the opposite is true.
 -}
----------
{-# LANGUAGE  ExistentialQuantification #-}
module Test (test) where
import qualified #{moduleName}
import Prelude
import Test.HUnit ((~:), assertFailure, Test)
import Data.List ((\\), sort)
import Control.Monad (unless)
import System.Random (randomRIO)

limit :: Num a => a
limit = 35000

test :: [ Test ]
test = concat
  [ [ " Test for duplicate occurrences in '" ++ name ++ "'" ~: do
        let dups = duplicates (take limit actual)
        unless (null dups) $ do
          let x = head dups
          assertFailure $ "At least the following element occurs more than once:\n"
                          ++ "    " ++ show x ++ "\n"
    , " Test for complete enumeration in '" ++ name ++ "'" ~: sequence_
      ( (unless (length (take limit actual) == limit)
         (assertFailure $ "The submitted list is finite and thus "
                          ++ "cannot be correct.")):
        [ do let missing = take n expected \\ take limit' actual
             unless (null missing) $ do
               x <- randomChoice missing
               let prefix = init (show (take 10 actual)) ++ ",...]"
               assertFailure $ "It seems not all elements are enumerated. "
                               ++ "For example, the following element does not occur "
                               ++ "among the first " ++ show limit' ++ " of the "
                               ++ "submission " ++ prefix ++ "\n"
                               ++ "(although in a correct submission it should "
                               ++ "with high probability):\n"
                               ++ "    " ++ show x ++ "\n"
        | n <- candidates (floor ((limit/factor)**(1/expon)))
        , let limit' = floor (factor * (fromIntegral n)**expon)
        ] ) ]
  | X name actual expected factor expon <-
      [ X "ints"     #{moduleName}.ints     ints     2  1.5
      , X "pairs"    #{moduleName}.pairs    pairs    2  2.5
      , X "triples"  #{moduleName}.triples  triples  3  2.5
      ]
  ] ++
  [ " Test for negative numbers in 'pairs'" ~:
      let negs = filter (\(x,y) -> x<0 || y<0) (take limit #{moduleName}.pairs)
      in unless (null negs) $
         assertFailure $ "At least the following pair contains a negative number:\n"
                         ++ "    " ++ show (head negs) ++ "\n"
  , " Test for negative numbers in 'triples'" ~:
      let negs = filter (\(x,y,_) -> x<0 || y<0) (take limit #{moduleName}.triples)
      in unless (null negs) $
         assertFailure $ "At least the following triple contains a negative number "
                         ++ "in an inappropriate place:\n    " ++ show (head negs) ++ "\n"
  ]

candidates :: Int -> [Int]
candidates n = reverse (go n) where
   go k  | k > 2*factor  = k : go (k `div` factor)
         | otherwise     = []
   factor = 3

duplicates :: (Ord a) => [a] -> [a]
duplicates xs = let xs' = sort xs
                in map fst $ filter (uncurry (==)) $ zip xs' (tail xs')

data X = forall a. (Show a, Ord a) => X String [a] [a] Double Double

ints :: [Integer]
ints = 0 : concat [[n, -n] | n <- [1..]]

pairs :: [(Integer,Integer)]
pairs = concat [[(x,s-x) | x <- [0..s]] | s <- [0..]]

triples :: [(Integer,Integer,Integer)]
triples = [(x,y,z) | (i,j) <- pairs, let (x,y) = ind pairs i,
                     let z = ind ints j]

ind :: [a] -> Integer -> a
ind (a:_)  0 = a
ind (_:as) n = ind as (n-1)
ind []     _ = error "IMPOSSIBLE!"

randomChoice :: [a] -> IO a
randomChoice xs = do
        r <- randomRIO (0, (length xs - 1))
        return $ xs !! r
