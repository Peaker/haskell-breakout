{-# LANGUAGE NamedFieldPuns #-}

module Physics
  ( moveBall
  , speedUp
  , movePaddle
  , computeBallDots
  , wallsBounce
  , bricksBounce
  , isGameOver
  , paddleBounce
  , resetPaddleVel
  , moveItems
  , itemsBounce
  ) where

import CollisionDetection
import Data.List (sortOn)
import Data.Maybe
import GameBoard

import Control.Lens

import Maths

-- aliases
type Seconds = Float

-- Speed up the ball
speedUp :: Position -> Position
speedUp = mul (speedRatio, speedRatio)

-- | update the 3 dots position used to calculate the collision vectors
computeBallDots ::
     Game -- ^ current state game
  -> Game -- ^ game updated
computeBallDots game = set ballDots [dot1, dot2, dot3] game
  where
    dot1 = computeBallDot (game ^. ballLoc) (game ^. ballVel) ballRadius
    dot2 = computeBallDot (game ^. ballLoc) nv ballRadius
      where
        nv = matrixMultiplication ((0, 1), (-1, 0)) (game ^. ballVel)
    dot3 = computeBallDot (game ^. ballLoc) nv ballRadius
      where
        nv = matrixMultiplication ((0, -1), (1, 0)) (game ^. ballVel)

-- | Update the dot position with the velocity
computeBallDot ::
     Position -- ^ ball center
  -> Velocity -- ^ vector
  -> Radius -- ^ ball radius
  -> Position -- ^ dot position
computeBallDot (x, y) (vx, vy) radius = (ax + x, ay + y)
  where
    ax = (vx * radius) / normV
    ay = (vy * radius) / normV
    normV = sqrt (vx * vx + vy * vy)

-- | Update the ball position using its current velocity.
moveBall ::
     Seconds -- ^ The number of seconds since last Update
  -> Game -- ^ current game state
  -> Game -- ^ A new game state with an updated ball position
moveBall seconds game = over ballLoc (add delta) game
        -- Old locations and velocities
  where
    delta = mul (seconds, seconds) (game ^. ballVel)

-- | Update the paddle position aand stop it if it goes throught the wall
movePaddle ::
     Game -- ^ current game state
  -> Game -- ^ Game paddle position updated
movePaddle game
  = over (paddle . paddleLoc) move game
  where
    move (x, y)
          -- No step , no mouvement
      | vel == 0 = (x, y)
          -- Lefter than left wall, but trying to go right.
      | x - halfPaddle <= leftGameBorder && vel > 0 =
        (x + paddleStep * vel, y)
          -- You are going into the left wall
      | x - halfPaddle <= leftGameBorder && vel < 0 =
        (leftGameBorder + halfPaddle, y)
          -- Righter than right wall , but trying to go left.
      | x + halfPaddle >= rightGameBorder && vel < 0 =
        (x + (paddleStep * vel), y)
          -- You are going into the right wall
      | x + halfPaddle >= rightGameBorder && vel > 0 =
        (rightGameBorder - halfPaddle, y)
          -- Between the two walls
      | x - halfPaddle > leftGameBorder && x + halfPaddle < rightGameBorder =
        (x + (paddleStep * vel), y)
      | otherwise = (x, y)
    vel = game ^. paddle . paddleVel . _1
    halfPaddle = game ^. paddle . paddleWidth / 2
    leftGameBorder = -(gameWidth / 2) + wallWidth / 2
    rightGameBorder = gameWidth / 2 - wallWidth / 2
    paddleStep = 1

itemsBounce :: Game -> Game
itemsBounce game = go itemTypeList game
  where
    (itemsUpdated, itemTypeList) = itemsCollision (game ^. paddle) (game ^. items)
    go :: [ItemType] -> Game -> Game
    go [] game = set items itemsUpdated game
    go (PaddleExpander:xs) game = go xs (changeWidth (+10) game)
    go (PaddleMinifier:xs) game = go xs (changeWidth (subtract 10) game)
    changeWidth = over (paddle . paddleWidth)

-- | Update the items positions
moveItems ::
     Game -- ^ game to update
  -> Game -- ^ game updated
moveItems = over items (mapMaybe (moveItem itemVel))

-- | update item position
moveItem ::
     Velocity -- ^ item velocity
  -> Item -- ^ item to move
  -> Maybe Item -- ^ item with a new position
moveItem (velx, vely) i
  | y < -(gameHeight / 2) = Nothing
  | otherwise = Just i {itemPos = (x + velx, y + vely)}
  where
    (x, y) = itemPos i

--  | Update the ball velocity and bricks if the ball hits a brick
bricksBounce ::
     Seconds -- ^ Number of seconds since last update
  -> Game -- ^ current state pf the game
  -> Game -- ^ Game updated
bricksBounce s game =
  case bc of
    Nothing -> game
    Just (vx, vy) ->
      game
      { _bricks = bricksUpdated
      , _ballVel = speedUp (vx / s, vy / s)
      , _gameScore = addScore score
      , _items = itemsUpdated ++ itemLts
      }
  where
    (bc, bricksUpdated, itemsUpdated) =
      bricksCollision (vx * s, vy * s) (game ^. ballDots) (game ^. bricks)
    (vx, vy) = game ^. ballVel
    score = game ^. gameScore
    itemLts = game ^. items

-- | Detect collision on the paddle and change velocity and score
paddleBounce ::
     Seconds -- ^ second since last update
  -> Game -- ^ current game state
  -> Game -- ^ game updated
paddleBounce s game =
  case nws of
    Nothing -> game
    Just t -> over ballVel (onVel t) game
  where
    onVel t =
        add (paddleV * 50, 0) .
        mul (1/s, 1/s) . collisionToSpeed t . mul (s, s)
    paddleV = game ^. paddle . paddleVel . _1
    dots = game ^. ballDots
    nws =
      detectDotsCollision (mul (s, s) (game ^. ballVel)) dots $
      paddleToRectangle $ game ^. paddle

-- | Detect collision on the walls and change ball velocity
wallsBounce ::
     Seconds -- ^ seconds since last update
  -> Game -- ^ current game state
  -> Game -- ^ game updated
wallsBounce s game =
  case collisions of
    [] -> game
    (x:xs) -> over ballVel (collisionToSpeed x) game
  where
    dots = game ^. ballDots
    speed = mul (s, s) (game ^. ballVel)
    gameWalls =
      [ (wallUpPos, gameWidth, wallWidth)
      , (wallLeftPos, wallWidth, gameHeight)
      , (wallRightPos, wallWidth, gameHeight)
      ]
    collisions =
      sortOn fst . catMaybes $ (detectDotsCollision speed dots <$> gameWalls)

-- | reset paddle velocity when the mouse stops
resetPaddleVel ::
     Game -- ^ current game state
  -> Game -- ^ game updated
resetPaddleVel game =
  if game ^. mouseEvent
    then set mouseEvent False game
    else set (paddle . paddleVel) (0, 0) game

-- | Verify if the game is over (ball outside the game)
isGameOver ::
     Game -- ^ current game state
  -> Game -- ^ updated game
isGameOver game
  | y < -(gameHeight / 2) = set gameState GameOver game
  | otherwise = game
  where
    (_, y) = game ^. ballLoc
