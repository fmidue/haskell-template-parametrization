module Solution where
import Prelude hiding ((!!), head, tail, last, init, take, drop, takeWhile, dropWhile)

{- Write a variant of the 'filter' function, named 'takeWhile'.
 - Unlike 'filter', which reduces a given list to all elements of it
 - that satisfy a given predicate, 'takeWhile' should produce a list
 - of all elements of a given list up to (but not including) the first
 - element where the predicate is not satisfied; so for example:
 - takeWhile (<5) [1,2,5,4,3,6] should result in [1,2].
 -}

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []
