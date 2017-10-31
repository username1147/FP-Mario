module Util where

import Graphics.Gloss.Data.Point
import Types



getWidth :: Rectangle -> Float
getWidth (Rectangle (xTopLeft, _) (xBottomRight, _)) = xBottomRight - xTopLeft

getHeight :: Rectangle -> Float
getHeight (Rectangle (_, yTopLeft) (_, yBottomRight)) = yTopLeft - yBottomRight



-- Returns the 4 corners of the rectangle in topLeft, topRight,
-- bottomRight, bottomLeft order
getCorners :: Rectangle -> [Point]
getCorners (Rectangle topLeftCorner bottomRightCorner) = [topLeftCorner,
															topRightCorner,
															bottomRightCorner,
															bottomLeftCorner]
	where
		(left, top)			= topLeftCorner
		(right, bottom)		= bottomRightCorner
		topRightCorner 		= (right, top)
		bottomLeftCorner	= (left, bottom)


-- Adds the given point to the given rectangle, effectively shifting the rectangle
shiftRectangle :: Rectangle -> Point -> Rectangle
shiftRectangle (Rectangle topLeftCorner bottomRightCorner) point = newRectangle
	where
		newRectangle = Rectangle {
			topLeft = topLeftCorner + point,
			bottomRight = bottomRightCorner + point
		}
