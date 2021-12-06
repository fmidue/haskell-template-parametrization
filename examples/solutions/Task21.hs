module Solution where
import Prelude hiding (($))

-- Recall that we encode a game level as a function of type
-- (Integer,Integer) -> Integer
--
-- Write a function 'isLessFilledThan' that compares two levels over
-- coordinates ranging from -10 to 10, and determines if the first
-- level is equal to the second level but with possibly some pearls
-- already collected by a player.
--
-- That is, the function returns True exactly if for all coordinate
-- pairs the elements at that position are equal or the first level
-- has an air tile (integer code 4) and the second level has a pearl
-- (integer code 3) there.
--
-- Note that we want reflexivity (x `isLessFilledThan` x) to hold.
--
-- Mathematically speaking, we want to implement a partial order
-- (that sorts levels with the same structure according to their
-- location-aware pearl richness).
--
-- Implement such a function and give it an appropriate type
-- signature.

isLessFilledThan :: ((Integer,Integer) -> Integer) -> ((Integer,Integer) -> Integer) -> Bool
isLessFilledThan f g =
  and
      [ let a = f (x,y)
            b = g (x,y)
        in a == b
           || (a == 4 && b == 3)
      | x <- range, y <- range ]
  where range = [-10..10]


-- alternative solutions:

isLessFilledThan' :: ((Integer,Integer) -> Integer) -> ((Integer,Integer) -> Integer) -> Bool
isLessFilledThan' f g =
  null [ () | x <- [-10..10], y <- [-10..10], f (x,y) /= g (x,y), f (x,y) /= 4 || g (x,y) /= 3 ]

isLessFilledThan'' :: ((Integer,Integer) -> Integer) -> ((Integer,Integer) -> Integer) -> Bool
isLessFilledThan'' f g =
  and [ a == b || a == 4 && b == 3 | (a,b) <- zip (tabulated f) (tabulated g) ]
  where
    tabulated h = [ h (x,y) | x <- range, y <- range ]
    range = [-10..10]
