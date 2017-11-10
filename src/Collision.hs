module Collision where

import Graphics.Gloss.Data.Point
import Types
import Rectangle


-- This data type will be used to represent a collision. The penetration depth
-- is how deep the two objects penetrate each other. The collision normal is
-- the vector (represented as a Point) along which the returned impulse should
-- be applied.
data Collision = Collision { normal :: Point, penetrationDepth :: Double, impulse :: Double }
	deriving (Eq, Show)




-- Returns True/False if the given point lies inside the rectangle. If the point
-- lies exactly on a corner or edge, it is not considered "inside"
{-
pointInsideRect :: Point -> Rectangle -> Bool
pointInsideRect (xCoord, yCoord) rectangle = insideHorizontal && insideVertical
	where
		Rectangle topLeftCorner bottomRightCorner	= rectangle
		(xTopLeft, yTopLeft)			= topLeftCorner
		(xBottomRight, yBottomRight)	= bottomRightCorner
		insideHorizontal				= xTopLeft		< xCoord && xCoord < xBottomRight
		insideVertical					= yBottomRight	< yCoord && yCoord < yTopLeft
-}
pointInsideRect p@(x0, y0) (Rectangle (x1, y1) (x2, y2))
	= x0 > min x1 x2
	&& x0 < max x1 x2
	&& y0 > min y1 y2
	&& y0 < max y1 y2


-- Returns True/False if 2 Rectangles are intersecting each other (aka, colliding).
-- If 2 Rectangles have corners or edges exactly on top of each other, it is not
-- considered a collision for those corners/edges.

isCollision :: Rectangle -> Rectangle -> Bool
isCollision rect1 rect2 = (x1 < x2 + w2) && (x1 + w1 > x2) && (y1 < y2 + h2) && (y1 + h1 > y2)
	where
		(x1, y1)		= topLeft rect1
		(x2, y2)		= topLeft rect2
		w1				= getWidth rect1
		w2				= getWidth rect2
		h1				= getHeight rect1
		h2				= getHeight rect2

isCollision2 rect1 rect2
	= (not $ pointInsideRect p1 rect2)
	&& (not $ pointInsideRect p2 rect2)
	&& (not $ pointInsideRect p3 rect2)
	&& (not $ pointInsideRect p4 rect2)
	where
		p1@(xB, yB) = bottomLeft rect1 -- xB = xBottom
		p2@(xT, yT) = topRight rect1
		p3 = (xT, yB)
		p4 = (xB, yT)

-- Returns a Point object that resembles the displacement that needs to be applied
-- to the first rectangle to make sure that the two rectangles no longer collide
-- NOTE: This function assumes the two rectangles actually collide, as well as
-- the fact that the first rectangle will be the one that will have to move
-- handleCollision :: Object -> Object -> Collision
-- handleCollision obj1 obj2 = Collision { normal = (0, 0), penetrationDepth = 1.0, impulse = 2.0 }
-- 	where
-- 		Object p1 v1 = obj1
-- 		Object p2 v2 = obj2






