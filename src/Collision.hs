module Collision where

import Graphics.Gloss.Data.Point
import Types
import Rectangle


-- To indicate from what direction the rectangles collided (first 1 with 2nd one)
data Direction = DirLeft | DirUp | DirRight | DirDown | DirUnknown
	deriving (Eq, Enum, Show)

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
-- If 2 Rectangles have corners or edges exactly on top of each other, it is
-- also considered a collision.
isCollision :: Rectangle -> Rectangle -> Bool
isCollision rectA rectB =
	not (xLeftA > xRightB || xRightA < xLeftB || yTopA < yBottomB || yBottomA > yTopB)
	where
		(xLeftA, yBottomA)	= bottomLeft rectA
		(xRightA, yTopA)	= topRight rectA
		(xLeftB, yBottomB)	= bottomLeft rectB
		(xRightB, yTopB) 	= topRight rectB


-- Returns the collision displacement vector for the two rectangles A and B that
-- are assumed colliding. More specifically, it returns the displacement vector
-- that will have to be applied to rectangle A such that A and B no longer
-- collide.
getCollisionDisplacement :: Rectangle -> Rectangle -> Point -> Point
getCollisionDisplacement rectA rectB moveVecA  = (displacementX, displacementY)
	where
		-- We figure out the displacement vector by first determining what
		-- corners of rect A are inside rect B...
		-- TODO: get corners of rect A that are inside rect B

		-- Then, get the smallest vector (in length) from that intersecting
		-- corner of rect A, to the closest corner of rect B. This will represent
		-- our initial displacement vector (as an estimate). Applying this
		-- displacement vector to rect A *should* make sure that A and B are no
		-- longer colliding (mathematical proof omitted, but one should be able
		-- to see why this works geometrically). If multiple intersecting corners
		-- are found, then we get all the smallest vector combinations, and
		-- select the one vector for which the dot product between that vector
		-- and the moveVecA is lowest. This results in an initial displacement
		-- vector estimate that deviates from the reverse moveVecA as little as
		-- possible, and it should effectively ensure that the chosen vector
		-- is as close to the reverse moveVecA as possible, for now.
		-- TODO: Get smallest vector of intersecting corner A with corners of B
		-- TODO: If multiple, select one with lowest dot product with moveVecA

		-- Then, based on that smallest vector and the given moveVecA, we figure
		-- out how much we have to move back along the moveVecA (its reverse!),
		-- to ensure that A and B no longer collide. For this, we will need the
		-- dot products again, one for the x axis, and 1 for the y axis, since
		-- we need to project the reverse moveVecA onto the system spanned by
		-- the initial displacement vector, so we can calculate how far along
		-- the reverse moveVecA we have to move back.
		-- TODO: Determine how far we have to move back along reverse moveVecA
		displacementX = 0.0
		displacementY = 0.0
