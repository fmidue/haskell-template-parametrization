module Solution where

{- Define an appropriate function 'mult' for the data type definition
 - below:
 -}

data Nat = Zero | Succ Nat  deriving Show

mult :: Nat -> Nat -> Nat
mult _  Zero      = Zero
mult n1 (Succ n2) = n1 `add` mult n1 n2

add :: Nat -> Nat -> Nat
add Zero      n  = n
add (Succ n1) n2 = Succ (add n1 n2)
