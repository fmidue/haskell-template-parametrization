module Solution where
import Test.HUnit ((@?=),(~:),runTestTT,Test(TestList))

-- Define, for the following algebraic data type:

data Tree = Leaf Integer | Node Tree Integer Tree  deriving Show

-- three general functions:

midfix, prefix, postfix :: Tree -> [Integer]

midfix (Leaf x)     = [x]
midfix (Node l x r) = midfix l ++ [x] ++ midfix r

prefix (Leaf x)     = [x]
prefix (Node l x r) = [x] ++ prefix l ++ prefix r

postfix (Leaf x)     = [x]
postfix (Node l x r) = postfix l ++ postfix r ++ [x]

-- with different traversal strategies, as exemplarily shown here:

test :: [ Test ]
test = -- runTestTT $ TestList
   [ "midfix"  ~: midfix  (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5))
                    @?= [1, 2, 3, 4, 5]
   , "prefix"  ~: prefix  (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5))
                    @?= [4, 2, 1, 3, 5]
   , "postfix" ~: postfix (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5))
                    @?= [1, 3, 2, 5, 4]
   ]
