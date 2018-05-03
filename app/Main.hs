{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import EventHandler
import GameBoard
import Levels
import Lib
import Physics
import Rendering

import Control.Lens

-- Conflict with the type Vector in module Maths
import Graphics.Gloss hiding (Vector)

import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

-- | Window background Color
background :: Color
background = greyN 0.1

-- | update world
updateWorldIO :: Float -> World -> IO World
updateWorldIO s = pure . over _1 (update s)

-- | Update the game state
update ::
     Float -- ^ The number of seconds since last update
  -> Game -- ^ current game state
  -> Game -- ^ A new game state with an updated ball and paddles positions.
-- Game playing
update seconds game@Game {_gameState = Playing} =
  isGameOver .
  isLevelOver .
  itemsBounce .
  moveItems .
  resetPaddleVel .
  movePaddle .
  paddleBounce seconds .
  moveBall seconds .
  wallsBounce seconds . bricksBounce seconds . computeBallDots $
  game
-- Game in a main menu state / Game paused
update _ game = game

-- | load library
loadLibrary :: IO (Library Picture)
loadLibrary = traverse loadBMP libraryBmpPaths

-- | Window
window :: Display
window = InWindow "Haskell Breakout" (winWidth, winHeight) (offset, offset)

-- | Frames per second
fps :: Int
fps = 60

main :: IO ()
main = do
  library <- loadLibrary
  playIO
    window
    background
    fps
    (initialState, library)
    renderWorldIO
    handleKeysWorldIO
    updateWorldIO
