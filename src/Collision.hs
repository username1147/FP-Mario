module Collision where

import Types


-- This data type will be used to represent a collision. The penetration depth
-- is how deep the two objects penetrate each other. The collision normal is
-- the vector (represented as a Point) along which the returned impulse should
-- be applied.
data Collision = Collision { normal :: Point, penetrationDepth :: Double, impulse :: Double }
	deriving (Eq, Show)


getWidth :: Rectangle -> Double
getWidth (Rectangle topLeftCorner bottomRightCorner) = x bottomRightCorner - x topLeftCorner

getHeight :: Rectangle -> Double
getHeight (Rectangle topLeftCorner bottomRightCorner) = y topLeftCorner - y bottomRightCorner



-- Returns the 4 corners of the rectangle in topLeft, topRight,
-- bottomRight, bottomLeft order
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
pointInsideRect point rectangle = insideHorizontal && insideVertical
	where
		Rectangle topLeftCorner bottomRightCorner	= rectangle
		Point xCoord yCoord	= point
		insideHorizontal	= (x topLeftCorner)		< xCoord && xCoord < (x bottomRightCorner)
		insideVertical		= (y bottomRightCorner)	< yCoord && yCoord < (y topLeftCorner)



-- Returns True/False if 2 Rectangles are intersecting each other (aka, colliding).
-- If 2 Rectangles have corners or edges exactly on top of each other, it is not
-- considered a collision for those corners/edges.
isCollision :: Rectangle -> Rectangle -> Bool
isCollision rect1 rect2 = (x1 < x2 + w2) && (x1 + w1 > x2) && (y1 < y2 + h2) && (y1 + h1 > y2)
	where
		Point { x = x1, y = y1 }	= topLeft rect1
		Point { x = x2, y = y2 }	= topLeft rect2
		w1							= getWidth rect1
		w2							= getWidth rect2
		h1							= getHeight rect1
		h2							= getHeight rect2



-- Returns a Point object that resembles the displacement that needs to be applied
-- to the first rectangle to make sure that the two rectangles no longer collide
-- NOTE: This function assumes the two rectangles actually collide, as well as
-- the fact that the first rectangle will be the one that will have to move
handleCollision :: Object -> Object -> Collision
handleCollision obj1 obj2 = Collision { normal = Point { x = 0, y = 0 }, penetrationDepth = 1.0, impulse = 2.0 }
	where
		Object { objectPos = p1, objectVelocity = v1 } = obj1
		Object { objectPos = p2, objectVelocity = v2 } = obj2






