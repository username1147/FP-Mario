module Collision where

import Types


-- Returns the 4 corners of the rectangle in topleft, topright,
-- bottomright, bottomleft order
getCorners :: Rectangle -> [Point]
getCorners (Rectangle topLeftCorner bottomRightCorner) = [topLeftCorner,
															topRightCorner,
															bottomRightCorner,
															bottomLeftCorner]
	where
		Point { x = left, y = top }			= topLeftCorner
		Point { x = right, y = bottom }		= bottomRightCorner
		topRightCorner 						= Point { x = right, y = top }
		bottomLeftCorner					= Point { x = left, y = bottom }



-- Returns True/False if the given point lies inside the rectangle. If the point
-- lies exactly on a corner or edge, it is not considered "inside"
pointInsideRect :: Point -> Rectangle -> Bool
pointInsideRect (Point xCoord yCoord) (Rectangle topLeftCorner bottomRightCorner) = insideHorizontal && insideVertical
	where
		insideHorizontal	= (x topLeftCorner) < xCoord && xCoord < (x bottomRightCorner)
		insideVertical		= (y bottomRightCorner) < yCoord && yCoord < (y topLeftCorner)



-- Returns True/False if 2 Rectangles are intersecting each other (aka, colliding).
-- If 2 Rectangles have corners or edges exactly on top of each other, it is not
-- considered a collision.
isCollision :: Rectangle -> Rectangle -> Bool
isCollision rect1 rect2 = any (== True) (cornerInside1 ++ cornerInside2)
	where
		corners1		= getCorners rect1
		corners2		= getCorners rect2
		mapFunc			= map (uncurry pointInsideRect)
		cornerInside1 = mapFunc (zip corners1 (replicate 4 rect2))
		cornerInside2 = mapFunc (zip corners2 (replicate 4 rect1))
