{-# LANGUAGE StandaloneDeriving #-}
module #{moduleName} where
import Data.List
import Data.Maybe

import #{otherTask1} (Pos, Player, xPlayer, oPlayer)
import #{otherTask2} (GameTree(..))

{- Let's continue our series of TicTacToe tasks.
 -
 - The goal is now, based on the 'GameTree' structure from the
 - preceding task (#{otherTask2}), to determine the best possible move and
 - the winning prospects of that move, for a player.
 -
 - So, for a given player and non-empty list of pairs (as in the
 - 'Continue' constructor) of possible moves for that player and the
 - game trees (for the opposing player) that would result from them,
 - you should return a position (or any single position of multiple
 - equally good positions) the given player should put his mark on, to
 - reach the, in that situation, best possible outcome for oneself.
 -
 - Furthermore you should return a value which represents the, in that
 - situation, best possible enforceable outcome from the perspective
 - of that player:
 -
 -   -> Nothing, for a draw
 -   -> Just player, for a win (if it's the player oneself)
 -                   or for losing (if it's the other player)
 -}

bestMove :: Player -> [ (Pos, GameTree) ] -> (Pos, Maybe Player)
bestMove player continue =
  let
    outcomes = [ (outcome, move) | (move, tree) <- continue
                                 , let outcome = case tree of
                                                   Draw -> Nothing
                                                   Lost -> Just player
                                                   Continue xs -> snd (bestMove (switch player) xs) ]
  in
    case lookup (Just player) outcomes of
      Just winning -> (winning, Just player)
      _ ->
        case lookup Nothing outcomes of
          Just drawing -> (drawing, Nothing)
          _ ->
            case head outcomes of
              (Just p, losing) | p /= player -> (losing, Just p)
              _ ->
                error "We know that this branch is never reached!"

switch :: Player -> Player
switch player | player == xPlayer = oPlayer
              | otherwise         = xPlayer

deriving instance Eq GameTree

{- Examples:
 - ----------
 -
 -    1. If it is xPlayer's turn and the list passed is as follows:
 -
 -      [ ((A,Z), Continue [ ((C,X), Continue [ ((C,Y), Draw) ]),
 -                         , ((C,Y), Continue [ ((C,X), Lost) ]) ])
 -      , ((C,X), Lost)
 -      , ((C,Y), Continue [ ((A,Z), Lost)
 -                         , ((C,X), Continue [ ((A,Z), Draw) ]) ]) ]
 -
 -    then 'bestMove' should return the pair ((C,X), Just xPlayer).
 -    Because if xPlayer plays (C,X), then it would next be oPlayer's
 -    turn, but that player would have lost. Neither of the other two
 -    possible moves for xPlayer are equally good. Because if xPlayer
 -    plays (A,Z), oPlayer could respond with (C,X), after which
 -    xPlayer could only play (C,Y), resulting in a draw. If however
 -    xPlayer were to play (C,Y) instead, then oPlayer could respond
 -    with (A,Z), forcing a lose for xPlayer.
 -
 -    2. If it is xPlayer's turn and the list passed is as follows:
 -
 -       [ ((C,X), Continue [ ((C,Y), Draw) ])
 -       , ((C,Y), Continue [ ((C,X), Lost) ]) ]
 -
 -    then bestMove should return the pair ((C,X), Nothing), because
 -    in this situation a draw is the best possible outcome for
 -    xPlayer. (After all, the move (C,Y) would lead to oPlayer
 -    winning in the next turn.)
 -
 -  Beware that situations are possible in which the current player,
 -  even when selecting only the best possible moves, will lose
 -  (assuming the opposing player is playing optimally). In this case
 -  you should still return a move, and in the second tuple component
 -  the information that the opponent will win.
 -
 -  Also when a draw or win are enforceable, there may be more than
 -  one possible move towards that goal; you should return any of
 -  those in that case.
 -}
