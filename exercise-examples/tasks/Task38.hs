enableWhitespaceWatermarking = return "True"
moduleName = return "Task38"
otherTask1 = return "Task30"
otherTask2 = return "Task34"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- incomplete-patterns
- incomplete-uni-patterns
- name-shadowing
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintErrors}
- Avoid lambda
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- "Use :"
- Use all
- Use and
- Use any
- Use camelCase
- Use concatMap
- Use const
- Use even
- Use guards
- Use if
- Use notElem
- Use odd
- Use or
- Use ||

allowAdding: true
allowModifying: false
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- missing-signatures

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Use ++
- Use 1
- Use catMaybes
- Use concat
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
# - Use null
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry
- Use unless
- Use when

#{commonConfigLanguageExtensions}
----------
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
bestMove = undefined

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
----------
module Test (test) where
import Prelude
import Data.List
import Data.Maybe
import Test.HUnit ((~:))
import qualified Test.HUnit as HU
import Test.QuickCheck

import TestHelper (qcWithTimeoutAndRuns)

import #{otherTask1} (Pos, Player, Board, xPlayer, oPlayer, initialBoard,
               possibleMoves, makeMove, endPosition)
import #{otherTask2} (GameTree (..), outlook, switch)
import qualified #{moduleName} (bestMove)

test :: [ HU.Test ]
test =
  [ " correct behaviour of bestMove?" ~:
      qcWithTimeoutAndRuns 5000 500
          $ forAll (validSituation >>= \(l,player,tree) ->
                     return $ INFO (l,player,tree, bestMoves (player, fromJust tree))) $
            \(INFO (l,player,tree,best))
            -> (l >= 6 && isJust tree)
               ==>  okay best (#{moduleName}.bestMove player (fromContinue $ fromJust tree))
  ]

fromContinue :: GameTree -> [(Pos,GameTree)]
fromContinue (Continue xs) = xs
fromContinue _ = error "IMPOSSIBLE!"

okay :: ([Pos], Maybe Player) -> (Pos, Maybe Player) -> Bool
okay (pos1,outcome1) (pos2,outcome2) = (elem pos2 pos1) && (outcome1 == outcome2)

validSituation :: Gen (Int, Player, Maybe GameTree)
validSituation = do n <- elements [6 .. 8 :: Int]
                    validPlay n 0 initialBoard xPlayer

validPlay :: Int -> Int -> Board -> Player -> Gen (Int, Player, Maybe GameTree)
validPlay n l board player =
  case endPosition board of
    Just _         -> return (l, player, Nothing)
    Nothing | n==0 -> return (l, player, Just $ outlook player board)
    Nothing | n>0  -> do move <- elements (possibleMoves board)
                         validPlay (n-1) (l+1) (makeMove player move board)
                                               (switch player)
    _ -> error "IMPOSSIBLE!"

-- switch :: Player -> Player
-- switch player | player == xPlayer = oPlayer
--               | otherwise         = xPlayer

showPlayer :: Player -> String
showPlayer player = if player == xPlayer then "xPlayer" else "oPlayer"

newtype INFO = INFO (Int,Player,Maybe GameTree,([Pos], Maybe Player))
instance Show INFO where
  show (INFO (_,player,Just tree,(moves,result))) =
    "(" ++ showPlayer player ++ "," ++ show tree ++ ")\n" ++
    "outcome should have been " ++
    (intercalate " or " [ "(" ++ show m ++ "," ++ (case result of {Nothing -> "Nothing"; Just pl -> "Just " ++ showPlayer pl}) ++ ")" | m <- moves ])
  show _ = error "IMPOSSIBLE!"

bestMoves :: (Player, GameTree) -> ([Pos], Maybe Player)
bestMoves (player, Continue cont)
  | won   /= Nothing  =  (winning, Just player)
  | draw  /= Nothing  =  (drawing, Nothing)
  | otherwise         =  let (ps,losing) = unzip outcomes
                         in case nub ps of
                              [Just p] | p /= player -> (losing, Just p)
                              _ -> error "IMPOSSIBLE!"
  where  won = lookups (Just player) outcomes
         winning = fromJust won
         draw = lookups Nothing outcomes
         drawing = fromJust draw
         outcomes = [ (outcome, move) |
                      (move, tree) <- cont,
                      let outcome = case tree of
                                      Draw  -> Nothing
                                      Lost  -> Just player
                                      _     -> snd $ bestMoves (switch player, tree) ]
bestMoves _ = error "IMPOSSIBLE!"

lookups :: Eq a => a -> [(a,b)] -> Maybe [b]
lookups _ []            = Nothing
lookups a ((a',b):rest) = if a/=a' then lookups a rest else
                          case lookups a rest of
                            Nothing -> Just [b]
                            Just bs -> Just (b:bs)

----------
module #{otherTask1} (Pos, Player, Board, xPlayer, oPlayer, initialBoard, (!),
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
----------
module #{otherTask2} (GameTree (..), outlook, switch) where
import Prelude
import Data.Maybe
import #{otherTask1} (Pos, Player, Board, xPlayer, oPlayer,
               possibleMoves, makeMove, endPosition)

switch :: Player -> Player
switch player | player == xPlayer = oPlayer
              | otherwise         = xPlayer

data GameTree = Lost | Draw | Continue [ (Pos, GameTree) ]  deriving Show

outlook :: Player -> Board -> GameTree
outlook player board =
  case (finished,winner) of
    (Nothing,_)               -> Continue [  (move, outlook (switch player)
                                                    $ makeMove player move board)
                                             | move <- possibleMoves board ]
    (_,Nothing)               -> Draw
    (_,Just p) | p /= player  -> Lost
    (_, _) -> error "IMPOSSIBLE!"
  where  finished     = endPosition board
         winner       = fromJust finished
