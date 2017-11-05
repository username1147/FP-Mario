module Rectangle where

import Graphics.Gloss.Data.Point

--------------------------
-- Rectangle manipulations
--------------------------
data Rectangle = Rectangle { bottomLeft :: Point, topRight :: Point }
	deriving (Eq, Show)


getWidth :: Rectangle -> Float
getWidth (Rectangle (xBottomLeft, _) (xTopRight, _)) = xTopRight - xBottomLeft

getHeight :: Rectangle -> Float
getHeight (Rectangle (_, yBottomLeft) (_, yTopRight)) = yTopRight - yBottomLeft

-- Returns the 4 corners of the rectangle in bottomLeft, topLeft,
-- topRight, bottomRight order
getCorners :: Rectangle -> [Point]
getCorners (Rectangle bottomLeftCorner topRightCorner) = [bottomLeftCorner,
															topLeftCorner,
															topRightCorner,
															bottomRightCorner]
	where
		(left, bottom)		= bottomLeftCorner
		(right, top)		= topRightCorner
		topLeftCorner 		= (left, top)
		bottomRightCorner	= (right, bottom)

-- Adds the given point to the given rectangle, effectively shifting the rectangle
shiftRectangle :: Rectangle -> Point -> Rectangle
shiftRectangle (Rectangle bottomLeftCorner topRightCorner) point = newRectangle
	where
		newRectangle = Rectangle {
			bottomLeft = bottomLeftCorner + point,
			topRight = topRightCorner + point
		}
