module #{moduleName} where
import Data.List
import Data.Maybe
import #{otherTask} (Row(..), Column(..), Pos, Player, Board,
               xPlayer, oPlayer, initialBoard,
               possibleMoves, makeMove, endPosition)

{- Let's continue the TicTacToe task. Assume that the implementations
 - from #{otherTask}, as imported above, are given. But you are not
 - supposed to have internal access to the 'Player' and 'Board'
 - representations here, instead you can only access them via the
 - imported functions.
 -
 - If you have no own correct solution for #{otherTask}, you can work
 - (locally) with the solution from #{collection}.
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
outlook player board =
  case endPosition board of
    Just Nothing -> Draw
    Just (Just _) -> Lost
    Nothing ->
      let freePos = possibleMoves board
      in Continue [ (position, outlook (switch player) newBoard)
                  | position <- freePos
                  , let newBoard = makeMove player position board
                  ]

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
 -   and it is xPlayer's turn, then a correct value of
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
