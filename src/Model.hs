module Model where

import Graphics.Gloss.Data.Point
-- new part
-- type Point = (Float, Float) -- for coordinates

data CameraInfo = CameraInfo { -- info about "camera"
                    cornerAbsPos :: Point, -- pos. of bottom-left corner
                    cameraHeight :: Int,
                    cameraWidth :: Int
                }

data RectObject = RectObject {
                    absCoords :: (Point, Point, Point, Point) -- mb list?
                }

data LevelMap = LevelMap {
                    landscape :: [RectObject],
                    mapHeight :: Int,
                    mapLength :: Int
                }

-- instance Num (Float, Float) where
--     (a, b) + (c, d) = (a + c, b + d)

absToRelCoord :: Point -> Point -> Point -- abs. pos -> screen abs. pos.
absToRelCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
-- absToRelCoord p1 p2 = p1 + p2

levelToScreenCoord :: Point -> Point -- because left-bottom of level has coord. (0, 0)
-- for the screen it is (-width/2, -height/2)
levelToScreenCoord (x, y) = (x - 200, y - 200) -- assuming screen is 400x400


-- now create some instances
rect1 :: RectObject
rect1 = RectObject ((0, 0), (0, 50), (500, 50), (500, 0))

rect2 :: RectObject
rect2 = RectObject ((300, 350), (300, 400), (800, 400), (800, 350))

sampleLevel :: LevelMap
sampleLevel = LevelMap [rect1, rect2] 400 800

-- old part 
data InfoToShow = ShowNothing
                | ShowSquare Point
    deriving Eq

data GameState = GameState {
                    infoToShow :: InfoToShow,
                    cameraInfo :: CameraInfo,
                    elapsedTime :: Float
                }

initialState :: GameState
initialState = GameState (ShowSquare (200, 200)) (CameraInfo (0, 0) 400 400) 0

nO_SECS_BETWEEN_CYCLES :: Float -- what is that for???
nO_SECS_BETWEEN_CYCLES = 5
