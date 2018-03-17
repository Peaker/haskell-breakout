module Main where

import  EventHandler
import  GameBoard
import  Physics
import  Rendering

-- Conflict with the type Vector in module Maths
import  Graphics.Gloss hiding (Vector)


import  Graphics.Gloss.Data.ViewPort

-- | Window background Color
background :: Color
background = black

-- | Update the game by moving the ball and bouncing off walls.
update :: Float   -- ^ The number of seconds since last update
       -> Game    -- ^ The initial game state
       -> Game    -- ^ A new game state with an updated ball and paddles positions.

-- Game playing
update seconds game @ Game { gameState = Playing }
      = isGameOver
      . movePaddle
      . bricksBounce seconds
      . moveBall seconds
      . collisionBounce seconds
      . computeDot $ game

-- Game paused
update seconds game @ Game { gameState = Paused } = game

-- Game paused
update seconds game @ Game { gameState = MainMenu } = game

-- Game over
update seconds game @ Game { gameState = GameOver } = game


-- | Window
window :: Display
window = InWindow "Haskell Breakout" (winWidth, winHeight) (offset, offset)

-- | Frames per second
fps :: Int
fps = 60

-- | Main
main :: IO ()
main = play window background fps initialState renderGame handleKeys update
