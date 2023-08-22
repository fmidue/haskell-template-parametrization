module #{moduleName} where
import Prelude hiding (($), (!!), take, drop, splitAt, elem, notElem, length, reverse, last, foldl, foldr)
import Test.QuickCheck

{-
Write a function 'fibSum' for the given type signature that computes

1 * x1 + 2 * x2 + 3 * x3 + 4 * x5 + 5 * x8 + ... ,

for a given list [x1,x2,x3,x4,x5,x6,x7,x8,...,xn].

That is, it computes the sum of j * xi where i <= n is the j-th
fibonacci number. You can make use of the isFib function given below.

(For this task, the fibonacci numbers start with 1 and 2, see fibList
below.)

Do not use any form of recursion to solve the task!
-}

-- To convince yourself that the following definition is working, you
-- might want to copy it into a different file, load that file into
-- ghci, and run something like 'take 100 fibList'.
fibList :: [Integer]
fibList = 1 : 2 : [ a + b | (a, b) <- zip fibList (tail fibList) ]

-- The following function computes the smallest fibonacci number that
-- is greater or equal to n.
nextFib :: Integer -> Integer
nextFib n = head [ m | m <- fibList, m >= n ]

-- The following function is what you probably want to use.
isFib :: Integer -> Bool
isFib n = n == nextFib n

fibSum :: [Integer] -> Integer
fibSum xs
  = sum [ i * v | (i,v) <- zip [1..] fibs ]
  where
    fibs = [ x | (j,x) <- zip [1..] xs, isFib j ]

-- alternative solution
fibSum' :: [Integer] -> Integer
fibSum' xs
  = sum [ i * v | (i,v) <- zip [1..] fibs ]
  where
    fibs = [ x | (True, x) <- zip [ isFib j | j <- [1..] ] xs ]

main :: IO ()
main = do
  putStrLn "Two simple unit tests"
  quickCheck (fibSum [1 .. 20] == 1 + 2 * 2 + 3 * 3 + 4 * 5 + 5 * 8 + 6 * 13)
  quickCheck (fibSum [20, 19 .. 1] == 1 * 20 + 2 * 19 + 3 * 18 + 4 * 16 + 5 * 13 + 6 * 8)
