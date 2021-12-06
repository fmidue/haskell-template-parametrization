configGhcErrors:
- deprecation
- empty-enumerations
- identities
- name-shadowing
- overflowed-literals
- overlapping-patterns
- tabs
configHlintErrors:
- Avoid reverse
- Collapse lambdas
- Evaluate
- Length always non-negative
- Move brackets to avoid $
- Redundant $
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant guard
- Redundant id
- Redundant if
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant section
- Use !!
- Use &&
- Use /=
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use camelCase
- Use drop
- Use elem
- Use even
- Use fst
- Use guards
- Use head
- Use id
- Use if
- Use init
- Use last
- Use left fold instead of right fold
- Use list literal pattern
- Use odd
- Use otherwise
- Use product
- Use right fold instead of left fold
- Use snd
- Use sum
- Use take
- Use ||
- Used otherwise as a pattern
- Using all on tuple
- Using and on tuple
- Using any on tuple
- Using concat on tuple
- Using elem on tuple
- Using foldr on tuple
- Using length on tuple
- Using maximum on tuple
- Using minimum on tuple
- Using null on tuple
- Using or on tuple
- Using product on tuple
- Using sum on tuple
allowAdding: true
allowModifying: false
allowRemoving: false
configHlintGroups:
- monomorphic
- teaching
# QuickCheck/HUnit testing follows the template check
configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds
configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
configHlintSuggestions:
- Apply De Morgan law
- Avoid lambda
- Avoid lambda using `infix`
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
- Use ++
- Use 1
- "Use :"
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
# - Use isJust
# - Use isNothing
- Use lefts
- Use list comprehension
- Use map
- Use map once
- Use mapMaybe
- Use maximum
# - Use maybe
- Use minimum
- Use negate
- Use newtype instead of data
- Use notElem
# - Use null
- Use or
- Use repeat
- Use replicate
- Use rights
- Use section
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
- Use unless
- Use when
configLanguageExtensions:
- NoTemplateHaskell
- TupleSections
# configLanguageExtensions - this sets LanguageExtensions for hlint as well
# configHlintSuggestions   - hlint hints to provide
# configHlintErrors        - hlint hints to enforce
# configGhcWarnings        - GHC warnings to provide as hints
# configGhcErrors          - GHC warnings to enforce
----------
module Solution where
import Data.List
import Data.Maybe
import Task30 (Row(..), Column(..), Pos, Player, Board,
               xPlayer, oPlayer, initialBoard,
               possibleMoves, makeMove, endPosition)

{- Let's continue the TicTacToe task. Assume that the implementations
 - from Task 30, as imported above, are given. But you are not
 - supposed to have internal access to the 'Player' and 'Board'
 - representations here, instead you can only access them via the
 - imported functions.
 -
 - If you have no own correct solution for Task 30, you can work
 - (locally) with the solution from collection.pdf.
 -
 - Additionally the following helper function is given:
 -}

switch :: Player -> Player
switch player | player == xPlayer = oPlayer
              | otherwise         = xPlayer

{- Your task now is to generate a 'game tree' of all possible further
 - developments (moves and their outcomes), for a given player in a given
 - situation:
 -}

data GameTree = Lost | Draw | Continue [ (Pos, GameTree) ]  deriving Show

outlook :: Player -> Board -> GameTree
outlook = undefined

{- Here 'Lost' and 'Draw' express the corresponding outcome for the
 - given player in the given situation. If neither of these cases has
 - occurred yet, 'Continue' expresses a non-empty list of all pairs of
 - currently possible positions for the player to put his mark on and
 - the descriptions of their corresponding further developments, for
 - the opposing player whose turn it will then be.
 -
 - (Hint: The reason for there being no 'Win' constructor in the
 -        GameTree data type is that it can't be a player's turn if
 -        that player has already won. A win is only possible *after*
 -        one's own turn, but at that point it is already the opposing
 -        player's turn - who has lost.)
 -
 - Example:
 - ---------
 -
 -   If the current board is as follows:
 -
 -       X Y Z
 -     A:X O .
 -     B:X X O
 -     C:. . O
 -
 -   and it is xPlayer's turn, then the correct value of
 -   'outlook xPlayer exampleBoard' is:
 -
 -     Continue [ ((A,Z), Continue [ ((C,X), Continue [ ((C,Y), Draw) ])
 -                                 , ((C,Y), Continue [ ((C,X), Lost) ]) ])
 -              , ((C,X), Lost)
 -              , ((C,Y), Continue [ ((A,Z), Lost)
 -                                 , ((C,X), Continue [ ((A,Z), Draw) ]) ]) ]
 -
 -   Because when xPlayer's move is (C,X), then the opposing player
 -   has lost. However when xPlayer's move is (C,Y), then the
 -   following oPlayer's move can be (A,Z) and xPlayer will have lost,
 -   or oPlayer moves (C,X) instead and leaves xPlayer no other choice
 -   than to move (A,Z), which leads to a draw. And if xPlayer in the
 -   original situation marks (A,Z), then possible moves for oPlayer
 -   are either (C,X) or (C,Y) with their corresponding continuations.
 -
 -   For the correctness of the GameTree it is not relevant in what
 -   order subtrees are listed. So for the example above, the
 -   following would be correct as well:
 -
 -     Continue [ ((C,X), Lost)
 -              , ((C,Y), Continue [ ((C,X), Continue [ ((A,Z), Draw) ])
 -                                 , ((A,Z), Lost) ])
 -              , ((A,Z), Continue [ ... ]) ]
 -}

-- Here is the game board used above, for testing purposes:

exampleBoard :: Board
exampleBoard = initialBoard
               `andThen` makeMove xPlayer (B,Y)
               `andThen` makeMove oPlayer (A,Y)
               `andThen` makeMove xPlayer (B,X)
               `andThen` makeMove oPlayer (B,Z)
               `andThen` makeMove xPlayer (A,X)
               `andThen` makeMove oPlayer (C,Z)
  where andThen = flip ($)
-------------------------------------
module Test (test) where
import Prelude
import Data.List
import Data.Maybe (fromJust)
import TestHelper (qcWithTimeoutAndRuns)
import Test.HUnit (Test,(@?=),(~:),assert)
import Test.QuickCheck

import Task30 (Player, Board, xPlayer, oPlayer, initialBoard,
               possibleMoves, makeMove, endPosition)
import Solution (switch)
import qualified Solution

test :: [Test]
test =
  [ " correct behaviour of outlook?" ~:
      qcWithTimeoutAndRuns 5000 350 $ forAll (validSituation >>= \(l,board,player) ->
                    return $ INFO (l,board,player,outlook player board)) $
           \(INFO (l,board,player,tree))
           -> l >= 6  ==>  sameOutlook tree (Solution.outlook player board)
  ]

validSituation :: Gen (Int, Board, Player)
validSituation = do n <- elements [6 .. 9 :: Int]
                    validPlay n 0 initialBoard xPlayer

validPlay :: Int -> Int -> Board -> Player -> Gen (Int, Board, Player)
validPlay n l board player =
  case endPosition board of
    Just _         -> return (l, board, player)
    Nothing | n==0 -> return (l, board, player)
    Nothing | n>0  -> do move <- elements (possibleMoves board)
                         validPlay (n-1) (l+1) (makeMove player move board)
                                               (switch player)
    Nothing -> error "impossible!"

sameOutlook :: Solution.GameTree -> Solution.GameTree -> Bool
sameOutlook Solution.Lost                 Solution.Lost                 = True
sameOutlook Solution.Draw                 Solution.Draw                 = True
sameOutlook (Solution.Continue outcomes1) (Solution.Continue outcomes2) =
  let outcomes2' = sortBy (\(m,_) (m',_) -> compare m m') outcomes2
  in (length outcomes1 == length outcomes2') &&
     all (\((m1,out1),(m2,out2)) -> m1==m2 && sameOutlook out1 out2)
         (zip outcomes1 outcomes2')
sameOutlook _                    _                    = False

newtype INFO = INFO (Int,Board,Player,Solution.GameTree)
instance Show INFO where
  show (INFO (_,board,player,tree)) =
    show player ++ "'s turn" ++ show board ++
    "outcome should have been (modulo reordering of lists): " ++ show tree

outlook :: Player -> Board -> Solution.GameTree
outlook player board =
  case (finished,winner) of
    (Nothing,_)               -> Solution.Continue
                                          [ (move, outlook (switch player) $
                                                   makeMove player move board)
                                          | move <- possibleMoves board ]
    (_,Nothing)               -> Solution.Draw
    (_,Just p) | p /= player  -> Solution.Lost
    (Just _,Just _)           -> error "impossible!"
  where  finished = endPosition board
         winner = fromJust finished
-------------------------------------
module Task30 (Row(..), Column(..), Pos, Player, Board, xPlayer, oPlayer, initialBoard, (!),
               possibleMoves, makeMove, endPosition) where
import Data.Maybe
import Data.List

data Row    = A | B | C deriving (Show, Read, Eq, Ord, Enum, Bounded)
data Column = X | Y | Z deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Pos = (Row, Column)

newtype Player = Player Bool  deriving Eq
newtype Board  = Board [ [ Maybe Player ] ]

xPlayer :: Player
xPlayer = Player True

oPlayer :: Player
oPlayer = Player False

instance Show Player where
  show (Player b) = if b then "xPlayer" else "oPlayer"

instance Show Board where
  show (Board ms) =
    "\n  X Y Z\n" ++
    (unlines $ map (\(r,l) ->
                     show r ++ ":" ++
                     (intersperse ' ' $ map (\e -> case e of
                                                {Nothing -> '.';
                                                 Just (Player True) -> 'X';
                                                 Just (Player False) -> 'O'})
                                      $ l))
             $ zip [A ..] ms)

initialBoard :: Board
initialBoard = Board $ replicate 3 $ replicate 3 Nothing

infixl 9 !
(!) :: Board -> Pos -> Maybe Player
(Board board) ! (row, column) = board !! fromEnum row !! fromEnum column

possibleMoves :: Board -> [Pos]
possibleMoves board = filter (isNothing . (board !)) moves

makeMove :: Player -> Pos -> Board -> Board
makeMove player move board | isNothing (board ! move)
                           = Board [ [ if (row, column) == move
                                         then Just player
                                         else board ! (row, column) | column <- elems ] | row <- elems ]
makeMove _ _ _ = error "IMPOSSIBLE!"

endPosition :: Board -> Maybe (Maybe Player)
endPosition board =  case nub [ player |  (m1,m2,m3) <- toTest,
                                          let player = board ! m1, isJust player,
                                          player == board ! m2,
                                          player == board ! m3]
                     of
                       []       -> if null (possibleMoves board) then Just Nothing else Nothing
                       [player] -> Just player
                       _:_:_    -> error "IMPOSSIBLE!"

toTest :: [((Row, Column), (Row, Column), (Row, Column))]
toTest = [ ((A,X),(A,Y),(A,Z)),
           ((A,X),(B,X),(C,X)),
           ((A,X),(B,Y),(C,Z)),
           ((A,Y),(B,Y),(C,Y)),
           ((A,Z),(B,Y),(C,X)),
           ((A,Z),(B,Z),(C,Z)),
           ((B,X),(B,Y),(B,Z)),
           ((C,X),(C,Y),(C,Z)) ]

elems :: (Bounded a, Enum a) => [a]
elems = [minBound .. maxBound]

moves :: [Pos]
moves = [ (row,column) | row <- elems, column <- elems ]
