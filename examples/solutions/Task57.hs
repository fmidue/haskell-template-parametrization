module Solution where
import Prelude hiding (replicate)

{- A Haskell function 'replicate' should generate a list containing
 - the same element (of any given type) n times (with n >= 0). Given
 - is the following type signature:
 -}

replicate :: Int -> a -> [a]

{- where the integer argument is n (the amount of entries in the
 - list).
 -
 - Write the complete function definition without using helper
 - functions:
 -}

replicate 0 _ = []
replicate n x = x : replicate (n-1) x
