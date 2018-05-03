{-# LANGUAGE DeriveTraversable #-}
module Rendering
  ( renderGame
  , Library(..), libraryBmpPaths
  ) where

import GameBoard

-- I want to use my own Vector.
import Graphics.Gloss hiding (Vector)

import Control.Lens hiding ((<.>))
import System.FilePath

-- | colors
scoreColor = orange

levelColor = orange

paddleColor = dark $ dark violet

stateText = orange

wallColor = violet

ballColor = orange

stateBGColor = black

itemColor = yellow

-- | Images library
data Library a = Library
  { brickImg :: a
  , mainMenuImg :: a
  , winImg :: a
  , gameOverImg :: a
  , nextLevelImg :: a
  , haskellLogoImg :: a
  , pausedImg :: a
  } deriving (Functor, Foldable, Traversable)

libraryNames :: Library String
libraryNames = Library
    { brickImg = "purpleBrick"
    , mainMenuImg = "mainMenu"
    , winImg = "win"
    , gameOverImg = "gameOver"
    , nextLevelImg = "nextLevel"
    , haskellLogoImg = "haskellLogo"
    , pausedImg = "paused"
    }

libraryBmpPaths :: Library FilePath
libraryBmpPaths =
    fmap toPath libraryNames
    where
        toPath name = "library" </> name <.> "bmp"

-- | Render score
renderScore ::
     Color -- ^ color
  -> Score -- ^ score to render
  -> Picture -- ^ picture of the score
renderScore c s =
  translate (-(gameWidth / 2) + 15) ((gameHeight / 2) - 50) $
  scale 0.15 0.15 $ color c $ Text ("Score : " ++ show s)

-- | Render level
renderGameLevel ::
     Color -- ^ color
  -> GameLevel -- ^ level to render
  -> Picture -- ^ picture of the level
renderGameLevel c l =
  translate ((gameWidth / 2) - 110) ((gameHeight / 2) - 50) $
  scale 0.15 0.15 $ color c $ Text ("Level : " ++ show l)

-- | render dot that indicate the potential hiting point
renderDot ::
     Color -- ^ dot color
  -> Radius -- ^ dot radius
  -> Position -- ^ dot position
  -> Picture -- ^ Picture of the dot
renderDot c radius (x, y) = translate x y $ color c $ circleSolid radius

-- | render state text
renderStateText ::
     Show a =>
     a -- ^ Text
  -> Position -- ^ Text position
  -> (Float, Float) -- ^ Scaling factors along X and Y dimensions.
  -> Picture -- ^ Picture of the text
renderStateText text (x, y) (sx, sy) =
  translate x y $ scale sx sy $ color stateText $ Text (show text)

-- | render wall
renderWall ::
     Color -- ^ Wall's color
  -> Width -- ^ Wall's width
  -> Height -- ^ Wall's height
  -> Position -- ^ Wall's center
  -> Picture -- ^ Wall's picture
renderWall col width height (x, y) =
  translate x y $ color col $ rectangleSolid width height

-- | render item
renderItem ::
     Color -- ^ item color
  -> Item -- ^ item to render
  -> Picture -- ^ item rendered
renderItem col item =
  translate x y $ color col $ rectangleSolid itemWidth itemHeight
  where
    (x, y) = itemPos item

-- | render ball
renderBall ::
     Color -- ^ Ball's color
  -> Radius -- ^ Ball's radius
  -> Position -- ^ Ball's center
  -> Picture -- ^ Picture of this ball
renderBall col radius (x, y) = translate x y $ color col $ circleSolid radius

-- | render brick
renderBrick ::
     Picture -- ^ brick image
  -> Brick -- ^ the brick to render
  -> Picture -- ^ brick picture
renderBrick p b = translate x y p
  where
    (x, y) = brickLoc b

-- render paddle
renderPaddle ::
     Color -- ^ the paddle color
  -> Width -- ^ paddle width
  -> Height -- ^ paddle height
  -> Position -- ^ paddle position
  -> Picture -- ^ paddle picture
renderPaddle c w h (x, y) = translate x y $ color c $ rectangleSolid w h

-- | render background
renderBackground ::
     Color -- ^ background color
  -> Picture -- ^ picture to render
renderBackground c = color c $ rectangleSolid gameWidth gameHeight

-- | render the game
renderGame ::
     Game -- ^ The game state to render
  -> Library Picture -- ^ image library
  -> Picture -- ^ A picture of this game state
-- MainMenu state
renderGame game library =
    case game ^. gameState of
    MainMenu -> mainMenuImg library
    GameOver ->
      pictures
        [ gameOverImg library
        , renderStateText (game ^. level) (-0, -175) (0.25, 0.25)
        , renderStateText (game ^. gameScore) (-20, -300) (0.25, 0.25)
        ]
    Win ->
      pictures
        [ winImg library
        , renderStateText (game ^. level) (-0, -175) (0.25, 0.25)
        , renderStateText (game ^. gameScore) (-20, -300) (0.25, 0.25)
        ]
    Paused -> pausedImg library
    NextLevel ->
      pictures
        [ nextLevelImg library
        , renderStateText (game ^. level) (0, -125) (0.25, 0.25)
        , renderStateText (game ^. gameScore) (0, -275) (0.25, 0.25)
        ]
    Playing ->
      pictures
        [ renderBall ballColor 10 (game ^. ballLoc)
        , renderWall wallColor gameWidth wallWidth wallUpPos
        , renderWall wallColor wallWidth (gameHeight + wallWidth * 2) wallLeftPos
        , renderWall wallColor wallWidth (gameHeight + wallWidth * 2) wallRightPos
        , renderPaddle paddleColor paddleW paddleHeight (game ^. paddle . paddleLoc)
        , pictures . fmap (renderBrick $ brickImg library) $ game ^. bricks
          -- , pictures . fmap (renderDot white 2) $ ballDots game
        , pictures . fmap (renderItem itemColor) $ game ^. items
        , renderScore scoreColor (game ^. gameScore)
        , renderGameLevel levelColor (game ^. level)
        ]
  where
    paddleW = game ^. paddle . paddleWidth
