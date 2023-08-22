enableWhitespaceWatermarking = return "True"
moduleName = return "Task30"
wikipedia = return "https://en.wikipedia.org/wiki/Tic-tac-toe"
----------
# the seed used was: #{seed}

#{commonConfigGhcErrors}
- name-shadowing

#{commonConfigHlintErrors}
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Use &&
- Use camelCase
- Use even
- Use guards
- Use if
- Use odd
- Use ||

allowAdding: true
allowModifying: true
allowRemoving: false

#{commonConfigHlintGroups}

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- unused-local-binds
- unused-matches
- unused-pattern-binds

#{commonConfigHlintRules}

#{commonConfigHlintSuggestions}
- Apply De Morgan law
- Avoid lambda
- Eta reduce
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
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
#- Use newtype instead of data
- Use notElem
# - Use null
- Use or
- Use repeat
- Use replicate
- Use rights
- Use splitAt
- Use sqrt
- Use tail
- Use tuple-section
# - Use uncurry

#{commonConfigLanguageExtensions}
----------
module #{moduleName} where
import Test.QuickCheck
import Data.Maybe
import Data.List
import qualified Data.Array as Array -- could be useful, but is optional
import qualified Data.Map as Map     -- could be useful, but is optional
-- also, deliberately not prevented import of (!!)

{- Imagine we want to implement the game TicTacToe:
 -
 -   #{wikipedia}
 -
 - To get going, let us first focus on the representation of the
 - game board and operations on it.
 -
 - Given are the following type definitions:
 -}

data Row    = A | B | C deriving (Show, Read, Eq, Ord, Enum, Bounded)
data Column = X | Y | Z deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Pos = (Row, Column)

{- Define some types 'Player' and 'Board' for distinguishing the two
 - participating players and for representing a concrete game board
 - situation:
 -}



{- If you define the type 'Player' by an algebraic data type, please
 - add 'deriving Eq' to its definition.
 -}

{- There should be two constants of type 'Player', here: -}

xPlayer :: Player
xPlayer = undefined

oPlayer :: Player
oPlayer = undefined

{- Now write definitions as requested in what follows. -}

-- representation of the empty game board:
initialBoard :: Board
initialBoard = undefined

-- lookup of a position in a given board:
--   board ! pos = Nothing      ==>  the position is still empty
--   board ! pos = Just player  ==>  the position was already taken
--                                   by the returned player
infixl 9 !
(!) :: Board -> Pos -> Maybe Player
(!) = undefined

-- free positions on a given board (no position returned twice):
possibleMoves :: Board -> [Pos]
possibleMoves = undefined

-- updating the game board with a move by one of the two players,
-- where you can assume that only acually legal moves (according to
-- 'possibleMoves') are passed:
makeMove :: Player -> Pos -> Board -> Board
makeMove = undefined

-- testing whether the game is finished, and with which result:
--   endPosition board = Nothing       ==>  game can still be continued
--   endPosition board = Just Nothing  ==>  game is finished, with a draw
--   endPosition board = Just (Just player), where either player == xPlayer
--                                                 or     player == oPlayer
--                                     ==>  game is finished, and the
--                                          returned player has won
--
-- (You can assume that only such boards are passed that can actually
--  occur during the course of the game, in particular no boards in
--  which both players each have claimed all three positions in a row,
--  column or main diagonal.)
endPosition :: Board -> Maybe (Maybe Player)
endPosition = undefined


-- An incomplete test suite:

main :: IO ()
main = do quickCheck $ \pos -> isNothing (initialBoard ! pos)
          quickCheck $ \pos -> pos `elem` possibleMoves initialBoard
          quickCheck $ isNothing (endPosition initialBoard)

{- The following definitions are only needed to help QuickCheck.
 - You can ignore them.
 -}

instance Arbitrary Row where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Column where
  arbitrary = elements [minBound .. maxBound]
----------
module Test (test) where
import qualified #{moduleName}
import Test.QuickCheck
import Test.HUnit ((~:), Test, assert, Assertion, assertFailure)
import Data.Maybe
import Data.List

import TestHelper (qcWithTimeoutAndRuns)

test1 :: #{moduleName}.Pos -> Bool
test1 pos = isNothing (#{moduleName}.initialBoard #{moduleName}.! pos)

test2 :: #{moduleName}.Pos -> Bool
test2 pos = pos `elem` #{moduleName}.possibleMoves #{moduleName}.initialBoard

test3 :: Bool
test3 = isNothing (#{moduleName}.endPosition #{moduleName}.initialBoard)

test :: [ Test ]
test =
  [ " xPlayer /= oPlayer ?"
    ~: assert $ #{moduleName}.xPlayer /= #{moduleName}.oPlayer
  , " initialBoard really empty (initialBoard ! pos = Nothing for all pos)?"
    ~: qcWithTimeoutAndRuns 5000 50 test1
  , " all moves possible at begin (elem pos (possibleMoves initialBoard) = True for all pos)?"
    ~: qcWithTimeoutAndRuns 5000 50 test2
  , " endPosition (initialBoard) = Nothing ?"
    ~: assert test3
  , " after arbitrary sequence of moves, correct determination of board entries using '!' ?"
    ~: qcWithTimeoutAndRuns 5000 100
          $ forAll (validSituation >>= \(moves,board,_) -> return $ HIDE2 (moves,board))
          $ \(HIDE2 (moves,board)) pos ->
               sameEntry (board ! pos)
                         ((execute moves #{moduleName}.initialBoard) #{moduleName}.! pos)
  , " after arbitrary sequence of moves, correct determination of possibleMoves?"
    ~: qcWithTimeoutAndRuns 5000 50
          $ forAll (validSituation
                    >>= \(moves,board,outcome)
                        -> return $ HIDE3 (moves,board,case outcome of
                                                         {Left _ -> False; _ -> True}))
          $ \(HIDE3 (moves,board,finished))
            -> (not finished) ==> (possibleMoves board) ==
               (sort (nub (#{moduleName}.possibleMoves (execute moves
                                                            #{moduleName}.initialBoard))))
  , " after arbitrary sequence of moves, no duplicates in output of possibleMoves?"
    ~: qcWithTimeoutAndRuns 5000 100
          $ forAll (validSituation
                    >>= \(moves,board,outcome)
                        -> return $ HIDE3 (moves,board,case outcome of
                                                         {Left _ -> False; _ -> True}))
          $ \(HIDE3 (moves,_,finished))
            -> (not finished) ==> let mvs = #{moduleName}.possibleMoves
                                            (execute moves #{moduleName}.initialBoard)
                                  in nub mvs == mvs
  , " after arbitrary sequence of moves, correct check using endPosition?"
    ~: qcWithTimeoutAndRuns 5000 100
          $ forAll (validSituation >>= \(moves,_,outcome)
                                       -> return $ INFO (moves,outcome))
          $ \(INFO (moves,outcome))
            -> sameOutcome outcome (#{moduleName}.endPosition
                                    (execute moves #{moduleName}.initialBoard))
  ]

elems :: (Bounded a, Enum a) => [a]
elems = [minBound .. maxBound]

validSituation :: Gen ([#{moduleName}.Pos], Board, Either Player (Maybe Player))
validSituation = do n <- elements [1..9 :: Int]
                    validPlay n [] initialBoard xPlayer

validPlay :: Int -> [#{moduleName}.Pos] -> Board -> Player -> Gen ([#{moduleName}.Pos], Board, Either Player (Maybe Player))
validPlay n ms board player =
  case endPosition board of
    Just outcome   -> return (reverse ms, board, Right outcome)
    Nothing | n==0 -> return (reverse ms, board, Left player)
    Nothing        -> do move <- elements (possibleMoves board)
                         validPlay (n-1) (move:ms) (makeMove player move board)
                                         (switch player)

switch :: Player -> Player
switch (Player player) = Player (not player)

execute :: [#{moduleName}.Pos] -> #{moduleName}.Board -> #{moduleName}.Board
execute = go True
  where go _      []        board = board
        go player (move:ms) board = go (not player) ms $
                                    #{moduleName}.makeMove
                                    (if player then #{moduleName}.xPlayer
                                               else #{moduleName}.oPlayer) move board

sameEntry :: Maybe Player -> Maybe #{moduleName}.Player -> Bool
sameEntry Nothing                Nothing        = True
sameEntry (Just (Player player)) (Just player') =
  if player then player'==#{moduleName}.xPlayer else player'==#{moduleName}.oPlayer
sameEntry _                      _              = False

sameOutcome :: Either a (Maybe Player) -> Maybe (Maybe #{moduleName}.Player) -> Bool
sameOutcome (Left _)                       Nothing               = True
sameOutcome (Right Nothing)                (Just Nothing)        = True
sameOutcome (Right (Just (Player player))) (Just (Just player')) =
  if player then player'==#{moduleName}.xPlayer else player'==#{moduleName}.oPlayer
sameOutcome _                              _                     = False

newtype HIDE2 = HIDE2 ([#{moduleName}.Pos],Board)
instance Show HIDE2 where
  show (HIDE2 (moves,_)) = show moves ++ " (starting with " ++ show xPlayer ++ ")"

newtype HIDE3 = HIDE3 ([#{moduleName}.Pos],Board,Bool)
instance Show HIDE3 where
  show (HIDE3 (moves,_,_)) = show moves ++ " (starting with " ++ show xPlayer ++ ")"

newtype INFO = INFO ([#{moduleName}.Pos], Either Player (Maybe Player))
instance Show INFO where
  show (INFO (moves,outcome)) = show moves ++ " (starting with " ++
                                show xPlayer ++ ")\n" ++
                                "outcome should have been: " ++
                                case outcome of
                                  Left  _   -> "Nothing"
                                  Right out -> show $ Just out

instance Show Player where
  show (Player b) = if b then "xPlayer" else "oPlayer"

newtype Player = Player Bool  deriving Eq
newtype Board = Board [ [ Maybe Player ] ]
xPlayer :: Player
xPlayer = Player True
-- oPlayer :: Player
-- oPlayer = Player False
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
             $ zip [#{moduleName}.A ..] ms)
initialBoard :: Board
initialBoard = Board $ replicate 3 $ replicate 3 Nothing
(!) ::  Board -> #{moduleName}.Pos -> Maybe Player
(Board board) ! (row, column) = board !! fromEnum row !! fromEnum column
possibleMoves :: Board -> [#{moduleName}.Pos]
possibleMoves board = filter ((== Nothing) . (board !))
                      [ (row,column) | row <- elems, column <- elems ]
makeMove :: Player -> #{moduleName}.Pos -> Board -> Board
makeMove player move board = Board [ [ if (row, column) == move
                                       then Just player
                                       else board ! (row, column)
                                       | column <- elems ] | row <- elems ]
endPosition :: Board -> Maybe (Maybe Player)
endPosition board = case nub [ player | (m1,m2,m3) <- to_test,
                                        let player = board ! m1, player /= Nothing,
                                        player == board ! m2,
                                        player == board ! m3 ]
                    of
                      []       -> if null (possibleMoves board)
                                  then Just Nothing
                                  else Nothing
                      [player] -> Just player
                      _        -> error "IMPOSSIBLE!"
to_test :: [((#{moduleName}.Row, #{moduleName}.Column), (#{moduleName}.Row, #{moduleName}.Column), (#{moduleName}.Row, #{moduleName}.Column))]
to_test = [ ((#{moduleName}.A,#{moduleName}.X),(#{moduleName}.A,#{moduleName}.Y),(#{moduleName}.A,#{moduleName}.Z)),
            ((#{moduleName}.A,#{moduleName}.X),(#{moduleName}.B,#{moduleName}.X),(#{moduleName}.C,#{moduleName}.X)),
            ((#{moduleName}.A,#{moduleName}.X),(#{moduleName}.B,#{moduleName}.Y),(#{moduleName}.C,#{moduleName}.Z)),
            ((#{moduleName}.A,#{moduleName}.Y),(#{moduleName}.B,#{moduleName}.Y),(#{moduleName}.C,#{moduleName}.Y)),
            ((#{moduleName}.A,#{moduleName}.Z),(#{moduleName}.B,#{moduleName}.Y),(#{moduleName}.C,#{moduleName}.X)),
            ((#{moduleName}.A,#{moduleName}.Z),(#{moduleName}.B,#{moduleName}.Z),(#{moduleName}.C,#{moduleName}.Z)),
            ((#{moduleName}.B,#{moduleName}.X),(#{moduleName}.B,#{moduleName}.Y),(#{moduleName}.B,#{moduleName}.Z)),
            ((#{moduleName}.C,#{moduleName}.X),(#{moduleName}.C,#{moduleName}.Y),(#{moduleName}.C,#{moduleName}.Z)) ]
