module Main where

import Control.Monad
import Control.Monad.Trans
import System.Console.ANSI
import System.Console.Haskeline
import System.IO
import UI.NCurses

import Lib

main :: IO ()
main = runCurses $ do
  setEcho False
  w <- defaultWindow
  gameloop w (Cursor 0 0) (Playing initBoard)

gameloop :: Window -> Cursor -> State -> Curses ()
gameloop w _ (AI_WON board)     = aiwon w (AI_WON board)
gameloop w _ (PLAYER_WON board) = playerwon w (PLAYER_WON board)
gameloop w _ (DRAW board)       = draw w (DRAW board)
gameloop w cursor state         = play w cursor state

play :: Window -> Cursor -> State -> Curses ()
play w (Cursor row column) (Playing board) = do
  mainColor <- newColorID ColorGreen ColorBlack 1
  playerColor <- newColorID ColorBlue ColorBlack 2
  aiColor <- newColorID ColorRed ColorBlack 3
  updateWindow w $ do
    drawHeading mainColor
    drawGameBoard mainColor
    drawTiles board playerColor aiColor
    drawInstructions mainColor
    drawCursor (Cursor row column)
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' | ev' == EventCharacter 'w' -> gameloop w (up (Cursor row column)) (Playing board)
    Just ev' | ev' == EventCharacter 'a' -> gameloop w (left (Cursor row column)) (Playing board)
    Just ev' | ev' == EventCharacter 's' -> gameloop w (down (Cursor row column)) (Playing board)
    Just ev' | ev' == EventCharacter 'd' -> gameloop w (right (Cursor row column)) (Playing board)
    Just ev' | ev' == EventCharacter 'q' -> return ()
    Just ev' | ev' == EventCharacter 'x' -> gameloop w (Cursor row column) (move board (Move row column PLAYER))
    Just ev'                             -> gameloop w (Cursor row column) (Playing board)

aiwon :: Window -> State -> Curses ()
aiwon w (AI_WON board) = do
  mainColor <- newColorID ColorGreen ColorBlack 1
  playerColor <- newColorID ColorBlue ColorBlack 2
  aiColor <- newColorID ColorRed ColorBlack 3
  updateWindow w $ do
    drawHeading mainColor
    drawGameBoard mainColor
    drawTiles board playerColor aiColor
    setColor aiColor
    moveCursor 16 16
    drawString "you've lost xD"
    setColor mainColor
    moveCursor 24 24
    drawString "Press q to quit ..."
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' ->
      if (ev' == EventCharacter 'q')
      then return ()
      else gameloop w (Cursor 0 0) (AI_WON board)

playerwon :: Window -> State -> Curses ()
playerwon w (PLAYER_WON board) = do
  mainColor <- newColorID ColorGreen ColorBlack 1
  playerColor <- newColorID ColorBlue ColorBlack 2
  aiColor <- newColorID ColorRed ColorBlack 3
  updateWindow w $ do
    drawHeading mainColor
    drawGameBoard mainColor
    drawTiles board playerColor aiColor
    setColor playerColor
    moveCursor 16 16
    drawString "you won ;)"
    setColor mainColor
    moveCursor 24 24
    drawString "Press q to quit ..."
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' ->
      if (ev' == EventCharacter 'q')
      then return ()
      else gameloop w (Cursor 0 0) (PLAYER_WON board)

draw :: Window -> State -> Curses ()
draw w (DRAW board) = do
  mainColor <- newColorID ColorGreen ColorBlack 1
  playerColor <- newColorID ColorBlue ColorBlack 2
  aiColor <- newColorID ColorRed ColorBlack 3
  drawColor <- newColorID ColorMagenta ColorBlack 4
  updateWindow w $ do
    drawHeading mainColor
    drawGameBoard mainColor
    drawTiles board playerColor aiColor
    setColor drawColor
    moveCursor 16 16
    drawString "you draw -_-"
    setColor mainColor
    moveCursor 24 24
    drawString "Press q to quit ..."
  render
  ev <- getEvent w Nothing
  case ev of
    Just ev' ->
      if (ev' == EventCharacter 'q')
      then return ()
      else gameloop w (Cursor 0 0) (DRAW board)

drawCursor :: Cursor -> Update ()
drawCursor (Cursor row column) = do
  moveCursor (toInteger (7 + row * 2)) (toInteger (26 + column * 4))

drawGameBoard :: ColorID -> Update ()
drawGameBoard color = do
  moveCursor 7 25
  setColor color
  drawString "   |   |   "
  moveCursor 8 25
  drawSeparator color
  moveCursor 9 25
  drawString "   |   |   "
  moveCursor 10 25
  drawSeparator color
  moveCursor 11 25
  drawString "   |   |   "

drawTiles :: Board -> ColorID -> ColorID -> Update ()
drawTiles ((Tile row column PLAYER):ts) playerColor aiColor = do
  moveCursor (toInteger (7 + row * 2)) (toInteger (26 + column * 4))
  setColor playerColor
  drawString "X"
  drawTiles ts playerColor aiColor
drawTiles ((Tile row column AI):ts) playerColor aiColor = do
  moveCursor (toInteger (7 + row * 2)) (toInteger (26 + column * 4))
  setColor aiColor
  drawString "O"
  drawTiles ts playerColor aiColor
drawTiles [] _ _ = return ()

drawInstructions :: ColorID -> Update ()
drawInstructions color = do
  -- | Instructions
  moveCursor 16 16
  setColor color
  drawString "Press x to make your move"
  moveCursor 18 16
  drawString "Press q to quit"

drawSeparator :: ColorID -> Update ()
drawSeparator color = do
  setColor color
  drawString "-----------"

drawTile :: Tile -> ColorID -> ColorID -> Update ()
drawTile (Tile row column PLAYER) playerColor _ = do
  setColor playerColor
  drawString " X "
drawTile (Tile row column AI) _ aiColor = do
  setColor aiColor
  drawString " O "

drawHeading :: ColorID -> Update ()
drawHeading color = do
  setColor color
  moveCursor 0 0
  drawString "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
  drawString "~~~~~~~~~~~~~~~~~~~~~~ Tic - Tac - Toe ~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
  drawString "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"

initBoard :: Board
initBoard = [ ]

