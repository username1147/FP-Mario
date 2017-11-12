module Collision where

import Graphics.Gloss.Data.Point
import Data.Foldable
import Rectangle
import Vector
import Types


-- Returns True/False if 2 Rectangles are intersecting each other (aka, colliding).
-- If 2 Rectangles have corners or edges exactly on top of each other, it is
-- not considered a collision.
isCollision :: Rectangle -> Rectangle -> Bool
isCollision rectA rectB =
	not (xLeftA >= xRightB || xRightA <= xLeftB || yTopA <= yBottomB || yBottomA >= yTopB)
	where
		(xLeftA, yBottomA)	= bottomLeft rectA
		(xRightA, yTopA)	= topRight rectA
		(xLeftB, yBottomB)	= bottomLeft rectB
		(xRightB, yTopB) 	= topRight rectB


-- Compares 2 tuples of format (length, point), and returns the ordering based
-- on the first element of the tuple, aka length.
compareFunc :: (Float, Point) -> (Float, Point) -> Ordering
compareFunc (length1, _) (length2, _)
	| length1 > length2		= GT
	| length1 == length2	= EQ
	| otherwise				= LT


-- Returns the collision displacement vector for the two rectangles A and B that
-- are assumed colliding. More specifically, it returns the displacement vector
-- that will have to be applied to rectangle A such that A and B no longer
-- collide. The moveVecA is the positional displacement of rectA (not speed!)
-- that may have caused the collision (as its basically the direction of movement
-- of rect A).
getCollisionDisplacement :: Rectangle -> Rectangle -> Point -> Point
getCollisionDisplacement rectA rectB moveVecA = finalDisplacement
	where
		-- We figure out the displacement vector by first determining what
		-- corners of rect A are inside rect B...
		cornersA		= getCorners rectA
		inside			= map (uncurry pointInsideRect) (zip cornersA (replicate 4 rectB))
		cornersInside	= [corner | (isInside, corner) <- zip inside cornersA, isInside]

		-- Then, get the smallest vector (in length) from that intersecting
		-- corner of rect A, to the closest edge of rect B. This will represent
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
		displacements	= [getDisplacement corner rectB | corner <- cornersInside]

		-- If multiple, select one with lowest dot product with moveVecA
		dotList				= [(dot moveVecA displacementVector, displacementVector) | displacementVector <- displacements]
		(_, displacement)	= minimumBy compareFunc dotList

		-- Then, based on that smallest vector and the given moveVecA, we figure
		-- out how much we have to move back along the moveVecA (its reverse!),
		-- to ensure that A and B no longer collide. For this, we will need the
		-- dot products again, one for the x axis, and 1 for the y axis, since
		-- we need to project the reverse moveVecA onto the system spanned by
		-- the initial displacement vector, so we can calculate how far along
		-- the reverse moveVecA we have to move back.
		reverseMoveVecA		= reverseVector moveVecA
		finalDisplacement	= projection displacement reverseMoveVecA
