module Solution (list) where
import Prelude hiding (($), (!!))

-- Write a "one-line" list comprehension implementing the following verbal
-- description.
--
-- Assume a positive integer constant c is given. Produce a list of all pairs
-- (x,y) of natural numbers such that all of the following hold:
--
--   * x and y are c-digit numbers
--   * y is at least twice as big as x
--   * the sum of x and y is also a c-digit number
--
-- A c-digit number is a natural number with exactly c digits.
-- E.g., the set of 2-digit numbers is {10,...,99}
--
-- The constant c could be changed by someone else (but not by you).
-- Your list definition should then still do the right thing.
--
-- No pair (x,y) should occur more than once in the list.

c :: Integer
c = 3

list :: [(Integer,Integer)]
list = [ (x,y) | x <- [10^(c-1)..10^c-1], y <- [2*x..10^c-1], x + y < 10^c ]
