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


topLeft :: Rectangle -> Point
topLeft (Rectangle (xBottomLeft, _) (_, yTopRight)) = (xBottomLeft, yTopRight)

bottomRight :: Rectangle -> Point
bottomRight (Rectangle (_, yBottomLeft) (xTopRight, _)) = (xTopRight, yBottomLeft)

-- Returns the 4 corners of the rectangle in bottomLeft, topLeft,
-- topRight, bottomRight order
getCorners :: Rectangle -> [Point]
getCorners rect = [bottomLeft rect, topLeft rect, topRight rect, bottomRight rect]

-- Returns the center of the given rectangle
getCenter :: Rectangle -> Point
getCenter (Rectangle bottomLeft topRight) = (centerX, centerY)
	where
		(x1, y1)	= bottomLeft
		(x2, y2)	= topRight
		centerX		= (x1 + x2) * 0.5
		centerY		= (y1 + y2) * 0.5

-- Returns the lowest x coordinate of the rectangle
getMinX :: Rectangle -> Float
getMinX (Rectangle (xBottomLeft, _) _) = xBottomLeft

-- Returns the highest x coordinate of the rectangle
getMaxX :: Rectangle -> Float
getMaxX (Rectangle _ (xTopRight, _)) = xTopRight

-- Returns the lowest y coordinate of the rectangle
getMinY :: Rectangle -> Float
getMinY (Rectangle (_, yBottomLeft) _) = yBottomLeft

-- Returns the highest y coordinate of the rectangle
getMaxY :: Rectangle -> Float
getMaxY (Rectangle _ (_, yTopRight)) = yTopRight


-- Adds the given point to the given rectangle, effectively shifting the rectangle
shiftRectangle :: Rectangle -> Point -> Rectangle
shiftRectangle (Rectangle bottomLeftCorner topRightCorner) point = newRectangle
	where
		newRectangle = Rectangle {
			bottomLeft = bottomLeftCorner + point,
			topRight = topRightCorner + point
		}
