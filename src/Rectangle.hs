module Rectangle where

import Graphics.Gloss.Data.Point
import Data.Foldable
import Vector

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

-- Returns the center x coordinate of the given rectangle
getCenterX :: Rectangle -> Float
getCenterX rect = fst $ getCenter rect

-- Returns the center y coordinate of the given rectangle
getCenterY :: Rectangle -> Float
getCenterY rect = snd $ getCenter rect



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



-- Returns True/False if the given point lies inside the rectangle. If the point
-- lies exactly on a corner or edge, it is not considered "inside"
pointInsideRect :: Point -> Rectangle -> Bool
pointInsideRect (x, y) (Rectangle bottomLeft topRight) = insideHorizontal && insideVertical
	where
		(xMin, yMin)		= bottomLeft
		(xMax, yMax)		= topRight
		insideHorizontal	= xMin	< x && x < xMax
		insideVertical		= yMin	< y && y < yMax


-- Returns a vector that returns the lowest displacement (vector length) of the
-- given point that is assumed to be inside the rect
getDisplacement :: Point -> Rectangle -> Point
getDisplacement (x, y) rect = displacement
	where
		-- Calculate distances from point (x, y) to edges of rect
		xLeftDistance			= x - getMinX rect
		xRightDistance			= x - getMaxX rect
		yBottomtDistance		= y - getMinY rect
		yTopDistance			= y - getMaxY rect

		-- List of all options
		options = [(xLeftDistance, 0.0), (xRightDistance, 0.0), (0.0, yBottomtDistance), (0.0, yTopDistance)]

		-- Comparison function to select smallest displacement
		compareFunc :: Point -> Point -> Ordering
		compareFunc x y
			| xLength > yLength		= GT
			| xLength == yLength	= EQ
			| otherwise				= LT
			where
				xLength = vectorLengthSquared x
				yLength = vectorLengthSquared y

		-- Select the one that has the lowest distance, aka lowest vector length
		displacement = minimumBy compareFunc options

-- Adds the given point to the given rectangle, effectively shifting the rectangle
shiftRectangle :: Rectangle -> Point -> Rectangle
shiftRectangle (Rectangle bottomLeftCorner topRightCorner) point = newRectangle
	where
		newRectangle = Rectangle {
			bottomLeft = bottomLeftCorner + point,
			topRight = topRightCorner + point
		}
