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
ints = 0 : concat [[n, -n] | n <- [1..]]

-- 2. the set of all pairs of natural numbers,
pairs :: [(Integer,Integer)]
pairs = concat [[(x,s-x) | x <- [0..s]] | s <- [0..]]

-- 3. the set of all triples of two natural numbers and an integer number.
triples :: [(Integer,Integer,Integer)]
triples = [(x,y,z) | (i,j) <- pairs, let (x,y) = pairs !! fromIntegral i, let z = ints !! fromIntegral j]

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
