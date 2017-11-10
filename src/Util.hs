module Util where

import Graphics.Gloss.Data.Point

--------------------------------------------------------------------------------
-- Rectangle manipulations
--------------------------------------------------------------------------------
data Rectangle = Rectangle { bottomLeft :: Point, topRight :: Point }
	deriving (Eq, Show)


getWidth :: Rectangle -> Float
getWidth (Rectangle (xBottomLeft, _) (xTopRight, _)) = xTopRight - xBottomLeft

getHeight :: Rectangle -> Float
getHeight (Rectangle (_, yBottomLeft) (_, yTopRight)) = yTopRight - yBottomLeft



-- Helper functions to get the other 2 corners of a Rectangle
topLeft :: Rectangle -> Point
topLeft (Rectangle (xBottomLeft, _) (_, yTopRight)) = (xBottomLeft, yTopRight)

bottomRight :: Rectangle -> Point
bottomRight (Rectangle (_, yBottomLeft) (xTopRight, _)) = (xTopRight, yBottomLeft)



-- Returns the 4 corners of the rectangle in bottomLeft, topLeft,
-- topRight, bottomRight order
getCorners :: Rectangle -> [Point]
getCorners rect@(Rectangle bottomLeftCorner topRightCorner) =
	[bottomLeftCorner, topLeft rect, topRightCorner, bottomRight rect]


-- Adds the given point to the given rectangle, effectively shifting the rectangle
shiftRectangle :: Rectangle -> Point -> Rectangle
shiftRectangle (Rectangle bottomLeftCorner topRightCorner) point = newRectangle
	where
		newRectangle = Rectangle {
			bottomLeft = bottomLeftCorner + point,
			topRight = topRightCorner + point
		}
