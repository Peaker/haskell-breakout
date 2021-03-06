module CollisionDetection
  ( detectDotsCollision
  , Speed
  , collisionToSpeed
  , bricksCollision
  , CollisionSide(..)
  , itemsCollision
  ) where

import Data.List (sortOn)
import Data.Maybe
import GameBoard
import Maths

-- | Collision side
data CollisionSide
  = LeftSide
  | TopSide
  | RightSide
  | BottomSide
  deriving (Show)

-- Aliases
type Speed = (Float, Float)

-- | calculate if there a hit between an item and the paddle, if so, it return
--   the items list and the hitten itemType list
itemsCollision ::
     Paddle -- ^ paddle
  -> [Item] -- ^ item list
  -> ([Item], [ItemType]) -- ^ item list and itemType list tuple
itemsCollision p items = go p items ([], [])
  where
    go :: Paddle -> [Item] -> ([Item], [ItemType]) -> ([Item], [ItemType])
    go _ [] (itemLts, itemTypeLts) = (itemLts, itemTypeLts)
    go p (i:items) (itemLts, itemTypeLts) =
      case collision of
        Nothing -> go p items (i : itemLts, itemTypeLts)
        Just hittenItem ->
          go p items (itemLts, itemType hittenItem : itemTypeLts)
      where
        collision = itemCollision p i

-- | Calculate if there a hit between an item and the paddle
itemCollision ::
     Paddle -- ^ Paddle
  -> Item -- ^ item
  -> Maybe Item -- ^ item updated
itemCollision p i =
  case collision of
    Just (_, _) -> Just i
    _ -> Nothing
  where
    collision =
      detectDotCollision
        (itemX, itemY - itemHeight / 2)
        itemV
        (paddleToRectangle p)
    itemV = itemVel
    (itemX, itemY) = itemPos i

-- | Calculate if there a hit between the ball and one of the brick
--   the system the list of bricks updated and the new speed of the ball
bricksCollision ::
     Speed -- ^ obje3ct speed (velocity * seconds since last update)
  -> [Position] -- ^ dot position
  -> [Brick] -- ^ list of bricks
  -> (Maybe Speed, [Brick], [Item]) -- ^ new speed with brick updated
bricksCollision ballSpeed dots bricks =
  go ballSpeed dots bricks (Nothing, [], [])
  where
    go ::
         Speed
      -> [Position]
      -> [Brick]
      -> (Maybe Speed, [Brick], [Item])
      -> (Maybe Speed, [Brick], [Item])
    go _ _ [] resp = resp
    go ballSpeed dots (brick:bs) (speed, brickLts, itemLts) =
      case collision of
        Nothing -> go ballSpeed dots bs (speed, brick : brickLts, itemLts)
        Just (t, collisionSide) ->
          ( Just $ collisionToSpeed (t, collisionSide) ballSpeed
          , brickLts ++ bs
          , case brickItem brick of
              Nothing -> itemLts
              Just t -> Item {itemType = t, itemPos = brickLoc brick} : itemLts)
      where
        collision = detectDotsCollision ballSpeed dots (brickToRectangle brick)

-- | Transform the t, collisionSide tuple to a speed
collisionToSpeed ::
     (Float, CollisionSide) -- ^ t, collisionSide tuple
  -> Speed -- ball speed
  -> Speed -- ball speed updated
collisionToSpeed (_, TopSide) (sx, sy) = (sx, -sy)
collisionToSpeed (_, BottomSide) (sx, sy) = (sx, -sy)
collisionToSpeed (_, RightSide) (sx, sy) = (-sx, sy)
collisionToSpeed (_, LeftSide) (sx, sy) = (-sx, sy)

-- |  detect if there a collision with a list of dots ans a rectangle
detectDotsCollision ::
     Speed -- ^ ball speed
  -> [Position] -- ^ ball dots
  -> Rectangle -- ^ renctangle
  -> Maybe (Float, CollisionSide) -- ^ time of collsion and collison side tuple
detectDotsCollision v positions rect =
  let mCollisions = map (\pos -> detectDotCollision pos v rect) positions
          -- Sort by ascending t
      collisions = sortOn fst . catMaybes $ mCollisions
  in case collisions of
       [] -> Nothing
       (x:xs) -> Just x

-- | defect collsion between a dot and a rectangle
detectDotCollision ::
     Position -- ^ dot position
  -> Speed -- ^ dot speed
  -> Rectangle -- ^ rectangle
  -> Maybe (Float, CollisionSide) -- ^ smallest t
                  -- (when the collision appears) and collisionSide
detectDotCollision dotPoint speed ((rectX, rectY), rectW, rectH) =
  let sides =
        [ (scalarProductTopSide, (rectW, 0), cornerTopLeft, TopSide)
        , (scalarProductBottomSide, (rectW, 0), cornerBottomLeft, BottomSide)
        , (scalarProductLeftSide, (0, -rectH), cornerTopLeft, LeftSide)
        , (scalarProductRightSide, (0, -rectH), cornerTopRight, RightSide)
        ]
      f (dotP, _, _, _)
        | dotP >= 0 = Nothing
      f (_, dirVec, startPoint, side) = do
        t <- intersecTime speed dotPoint dirVec startPoint
        return (t, side)
      collisions = sortOn fst $ mapMaybe f sides
  in case collisions of
       (x:xs) -> Just x
       _ -> Nothing
        -- scalar product with normal vector of rectangle top side
  where
    scalarProductTopSide = speed `dot` (0, 1)
        -- scalar product with normal vector of rectangle bottom side
    scalarProductBottomSide = speed `dot` (0, -1)
        -- scalar product with normal vector of rectangle left side
    scalarProductLeftSide = speed `dot` (-1, 0)
        -- scalar product with normal vector of rectangle right side
    scalarProductRightSide = speed `dot` (1, 0)
        -- rectangle corner top left point
    cornerTopLeft = (rectX - rectW / 2, rectY + rectH / 2)
        -- rectangle corner top right point
    cornerTopRight = (rectX + rectW / 2, rectY + rectH / 2)
        -- rectangle corner top bottom point
    cornerBottomLeft = (rectX - rectW / 2, rectY - rectH / 2)
