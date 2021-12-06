module Solution where
import Prelude hiding ((!!))

{- Implement insertion sort with 'foldr', using a helper function
 - 'insert' (which doesn't necessarily use 'foldr'):
 -}

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x < y = x:y:ys
  | otherwise = y : insert x ys

{- Here 'insert' should insert an element in the appropriate place of
 - an already sorted list.
 -}

example :: Bool
example = insert 4 [1, 3, 6, 7] == [1, 3, 4, 6, 7]


insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
