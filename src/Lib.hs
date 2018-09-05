module Lib
    ( Player(..)
    , Tile(..)
    , Board
    , Cursor(..)
    , Move(..)
    , State(..)
    , move
    , up
    , down
    , left
    , right
    ) where

import Data.List

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

data State = Playing Board
           | AI_WON Board
           | PLAYER_WON Board
           | DRAW Board
           deriving (Eq, Show)

data Cursor = Cursor Int Int deriving (Eq, Show)

data Player = PLAYER
            | AI
            deriving (Eq, Show)

data Move = Move Int Int Player deriving (Eq, Show)

data Tile = Tile Int Int Player deriving (Eq, Show)

type Board = [Tile]

move :: Board -> Move -> State
move board (Move row column player)
  | invalid board (Move row column player) == True = Playing board
  | otherwise                                      = (play board (Move row column player))
                                                     |> checkVictory
                                                     |> playAI
                                                     |> checkVictory

playAI :: State -> State
playAI (Playing board) = play board (nextAIMove board)
playAI state           = state

nextAIMove :: Board -> Move
nextAIMove board = validMoves board AI
                |> (\ moves -> zip moves (fmap (\ move -> minmax AI (play board move)) moves))
                |> minimumBy (\ (move, score) (move', score') -> case () of
                               _ | score < score'  -> LT
                                 | score == score' -> EQ
                                 | score > score'  -> GT)
                |> fst

data MoveScore = MoveScore Move Int deriving (Eq, Show)

minmax :: Player -> State -> Int
minmax _ (PLAYER_WON board)   = 1
minmax _ (AI_WON board)       = -1
minmax _ (DRAW board)         = 0
minmax PLAYER (Playing board) = validMoves board PLAYER
                             |> fmap (play board)
                             |> fmap checkVictory
                             |> fmap (minmax AI)
                             |> (-1:)
                             |> maximum
minmax AI (Playing board)     = validMoves board AI
                             |> fmap (play board)
                             |> fmap checkVictory
                             |> fmap (minmax PLAYER)
                             |> (1:)
                             |> minimum

validMoves :: Board -> Player -> [Move]
validMoves board player =
  [ Move 0 0 player, Move 0 1 player, Move 0 2 player
  , Move 1 0 player, Move 1 1 player, Move 1 2 player
  , Move 2 0 player, Move 2 1 player, Move 2 2 player ]
  |> filter (\ (Move row column p) -> not $ any (\ (Tile row' column' p') -> row == row' && column == column') board)

play :: Board -> Move -> State
play board (Move row column player) = Playing (Tile row column player : board)

checkVictory :: State -> State
checkVictory (Playing board)
  | playerWon board = PLAYER_WON board
  | aiWon board     = AI_WON board
  | draw board      = DRAW board
  | otherwise       = Playing board
checkVictory state  = state

playerWon :: Board -> Bool
playerWon board = wonOnRow board PLAYER || wonOnColumn board PLAYER || wonOnDiagonal board PLAYER

aiWon :: Board -> Bool
aiWon board = wonOnRow board AI || wonOnColumn board AI || wonOnDiagonal board AI

wonOnRow :: Board -> Player -> Bool
wonOnRow board player' =
  (> 0)
  $ length
  $ filter (\ x -> x == True)
  $ (\ row' -> (length (filter (\ (Tile row column player) -> row == row' && player == player') board)) == 3)
  <$> [0, 1, 2]

wonOnColumn :: Board -> Player -> Bool
wonOnColumn board player' =
  (> 0)
  $ length
  $ filter (\ x -> x == True)
  $ (\ column' -> (length (filter (\ (Tile row column player) -> column' == column && player == player') board) == 3))
  <$> [0, 1, 2]

draw :: Board -> Bool
draw board = (not $ playerWon board) && (not $ aiWon board) && (length board == 9)

wonOnDiagonal :: Board -> Player -> Bool
wonOnDiagonal board player =
  (length (filter (\ (Tile row column player') -> row == column && player == player') board) == 3) ||
  (length (filter (\ (Tile row column player') -> (column == - row + 2) && player == player') board) == 3)

invalid :: Board -> Move -> Bool
invalid board (Move row column player) = column < 0
                                      || column > 2
                                      || row < 0
                                      || row > 2
                                      || any (\(Tile row' column' player') ->
                                                row' == row && column' == column) board

up :: Cursor -> Cursor
up (Cursor row column)
  | row > 0   = Cursor (row - 1) column
  | otherwise = Cursor row column

down :: Cursor -> Cursor
down (Cursor row column)
  | row < 2   = Cursor (row + 1) column
  | otherwise = Cursor row column

left :: Cursor -> Cursor
left (Cursor row column)
  | column > 0 = Cursor row (column - 1)
  | otherwise  = Cursor row column

right :: Cursor -> Cursor
right (Cursor row column)
  | column < 2 = Cursor row (column + 1)
  | otherwise  = Cursor row column
  
