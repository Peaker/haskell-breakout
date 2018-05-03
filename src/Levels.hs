module Levels
  ( isLevelOver
  ) where

import GameBoard

import Control.Lens

-- | Checks if the bricks list is empty, next level

isLevelOver :: Game -> Game
isLevelOver game
  | null (game ^. bricks) =
  if isWin
    then set gameState Win game
    else newLevelState (gameLevel + 1) score
  where
    gameLevel = game ^. level
    score = game ^. gameScore
    isWin = gameLevel > 9
isLevelOver game = game
