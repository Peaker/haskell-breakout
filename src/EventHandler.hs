{-# LANGUAGE NamedFieldPuns #-}

module EventHandler
  ( handleKeysIO
  , Event(..)
  ) where

import GameBoard
import Physics

import Control.Lens
import Graphics.Gloss.Interface.Pure.Game
import System.Exit

-- | IO responding to key events
handleKeysIO :: Event -> Game -> IO Game
-- For an 'q' keypress, exit the game
handleKeysIO (EventKey (Char 'q') Up _ _) game = exitSuccess
handleKeysIO event game = return $ handleKeys event game

-- | Pure responding to key events.
handleKeys ::
     Event -- ^ keyEvent
  -> Game -- ^ current game state
  -> Game -- ^ Game updated
-- Cheat code
handleKeys (EventKey (Char 'n') Up _ _) game@Game {_gameState = Playing} =
  newLevelState (game ^. level + 1) 0
handleKeys (EventKey (Char 'w') Up _ _) game@Game {_gameState = Playing} =
  game {_gameState = Win}
-- For an 'Left' or 'Right' keypress, move verticaly player1 paddle
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game =
  set (paddle . paddleVel) (-1, 0) game
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game =
  set (paddle . paddleVel) (0, 0) game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game =
  set (paddle . paddleVel) (1, 0) game
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game =
  set (paddle . paddleVel) (0, 0) game
-- Moving mouse event , move verticaly the paddle
handleKeys (EventMotion (x, _)) game =
  game
  & set (paddle . paddleVel) (x - px, 0)
  & set mouseEvent True
  where
    (px, _) = game ^. paddle . paddleLoc
-- For an 'p' keypress, pause the game.
handleKeys (EventKey (Char 'p') Up _ _) game@Game {_gameState = Playing} =
  game {_gameState = Paused}
handleKeys (EventKey (Char 'p') Up _ _) game@Game {_gameState = Paused} =
  game {_gameState = Playing}
-- For an 'enter' keypress, start the game.
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) game@Game {_gameState = MainMenu} =
  game {_gameState = Playing}
-- Press any key to reset the game
handleKeys (EventKey _ Down _ _) game@Game {_gameState = GameOver} = initialState
handleKeys (EventKey _ Down _ _) game@Game {_gameState = Win} = initialState
-- Press any key to continue
handleKeys (EventKey _ Down _ _) game@Game {_gameState = NextLevel} =
  game {_gameState = Playing}
-- Do nothing for all other events.
handleKeys _ game = game
