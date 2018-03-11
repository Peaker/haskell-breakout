module Rendering
  ( mkWall
  , mkBall
  , mkBrick
  , renderGame
  , mkStateText
  ) where

import GameBoard

-- I want to use my own Vector.
import Graphics.Gloss hiding (Vector)


-- | render dot
renderDot :: Color    -- ^ dot color
          -> Position -- ^ dot position
          -> Radius   -- ^ dot radius
          -> Picture  -- ^ Picture of the dot
renderDot c (x, y) radius = translate x y $ color c $  circleSolid radius

-- | render state text
mkStateText :: Color      -- ^ Text color
            -> String     -- ^ Text
            -> Position   -- ^ Text position
            -> Picture    -- ^ Picture of the text
mkStateText col text (x, y) = translate (-120) 0 $ scale x y $ color col $ Text text

-- | render wall
mkWall :: Color           -- ^ Wall's color
       -> Width           -- ^ Wall's width
       -> Height          -- ^ Wall's height
       -> (Float, Float)  -- ^ Wall's center
       -> Picture         -- ^ Wall's picture
mkWall col width height (x , y) =
      translate x y $ color col $ rectangleSolid width height

-- | render ball
mkBall :: Color   -- ^ Ball's color
       -> Radius   -- ^ Ball's radius
       -> (Float, Float)  -- ^ Ball's center
       -> Picture         -- ^ Picture of this ball
mkBall col radius (x, y) = translate x y  $ color col $ circleSolid radius

-- | render brick
mkBrick :: Brick    -- ^ the brick to render
        -> Picture  -- ^ brick picture
mkBrick b = translate x y $ color (brickCol b) $ rectangleSolid brickWidth brickHeight
      where
        (x, y) = brickLoc b

-- render paddle
mkPaddle :: Color     -- ^ the paddle color
        -> Width      -- ^ paddle width
        -> Height     -- ^ paddle height
        -> Position   -- ^ paddle position
        -> Picture    -- ^ paddle picture
mkPaddle c w h (x, y) = translate x y $ color c $ rectangleSolid w h

-- | render the game
renderGame :: Game      -- ^ The game state to render
           -> Picture   -- ^ A picture of this game state
renderGame game = pictures
      [ mkBall (dark red) 10 (ballLoc game)
      , mkWall wallColor gameWidth wallWidth wallUpPos
      , mkWall wallColor gameWidth wallWidth wallDownPos
      , mkWall wallColor wallWidth gameHeight wallLeftPos
      , mkWall wallColor wallWidth gameHeight wallRightPos
      , pictures . fmap mkBrick $ bricks game
      , mkPaddle paddleColor paddleWidth paddleHeight (paddleLoc $ paddle game)
      , renderDot white (ballDot game) 2
      ]
      where
        wallColor = blue
        brickColor = yellow
        paddleColor = cyan
