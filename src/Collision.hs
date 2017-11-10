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

pointInsideRect :: Point -> Rectangle -> Bool
pointInsideRect (x, y) (Rectangle bottomLeft topRight) = insideHorizontal && insideVertical
	where
		(xMin, yMin)		= bottomLeft
		(xMax, yMax)		= topRight
		insideHorizontal	= xMin	< x && x < xMax
		insideVertical		= yMin	< y && y < yMax


-- Returns True/False if 2 Rectangles are intersecting each other (aka, colliding).
-- If 2 Rectangles have corners or edges exactly on top of each other, it is not
-- considered a collision for those corners/edges.

isCollision :: Rectangle -> Rectangle -> Bool
isCollision rectA rectB =
	not (xLeftA > xRightB || xRightA < xLeftB || yTopA < yBottomB || yBottomA > yTopB)
	where
		(xLeftA, yBottomA) = bottomLeft rectA
		(xRightA, yTopA) = topRight rectA
		(xLeftB, yBottomB) = bottomLeft rectB
		(xRightB, yTopB) = topRight rectB


-- Returns a Point object that resembles the displacement that needs to be applied
-- to the first rectangle to make sure that the two rectangles no longer collide
-- NOTE: This function assumes the two rectangles actually collide, as well as
-- the fact that the first rectangle will be the one that will have to move
-- handleCollision :: Object -> Object -> Collision
-- handleCollision obj1 obj2 = Collision { normal = (0, 0), penetrationDepth = 1.0, impulse = 2.0 }
-- 	where
-- 		Object p1 v1 = obj1
-- 		Object p2 v2 = obj2

