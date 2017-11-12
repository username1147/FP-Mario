module Collision where

import Graphics.Gloss.Data.Point
import Types
import Rectangle
import Data.Foldable


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


-- Returns the dot product between 2 vectors/points
dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- Returns the subtraction of 2 vectors/points
sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Returns the squared length of the vector
vectorLengthSquared :: Point -> Float
vectorLengthSquared x = dot x x

-- Returns the length of the vector
vectorLength :: Point -> Float
vectorLength x = sqrt $ vectorLengthSquared x

-- Returns the projection of vector A on vector B
projection :: Point -> Point -> Point
projection pA@(xA, yA) pB = (xA * scale, yA * scale)
	where
		scale = (dot pA pB) / (vectorLengthSquared pA)


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
-- collide.
getCollisionDisplacement :: Rectangle -> Rectangle -> Point -> Point
getCollisionDisplacement rectA rectB moveVecA  = (displacementX, displacementY)
	where
		-- We figure out the displacement vector by first determining what
		-- corners of rect A are inside rect B...
		-- TODO: get corners of rect A that are inside rect B
		cornersA		= getCorners rectA
		inside			= map (uncurry pointInsideRect) (zip cornersA (replicate 4 rectB))
		cornersInside	= [corner | (isInside, corner) <- zip inside cornersA, isInside]
		multipleCorners	= length cornersInside > 1

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
		cornersB			= getCorners rectB
		tempVectorsList		= [map (uncurry sub) (zip cornersB (replicate 4 cornerA)) | cornerA <- cornersInside]
		-- tempLengthsList		= [map vectorLengthSquared vectors | vectors <- tempVectors]
		tempTuplesList		= [ [(vectorLengthSquared vector, vector) | vector <- tempVectors] | tempVectors <- tempVectorsList]
		cornerTuples		= [minimumBy compareFunc tupleList | tupleList <- tempTuplesList]

		-- TODO: If multiple, select one with lowest dot product with moveVecA
		tempDotList			= [(dot moveVecA cornerVector, cornerVector) | (_, cornerVector) <- cornerTuples]
		(_, displacement)	= minimumBy compareFunc tempDotList

		-- Then, based on that smallest vector and the given moveVecA, we figure
		-- out how much we have to move back along the moveVecA (its reverse!),
		-- to ensure that A and B no longer collide. For this, we will need the
		-- dot products again, one for the x axis, and 1 for the y axis, since
		-- we need to project the reverse moveVecA onto the system spanned by
		-- the initial displacement vector, so we can calculate how far along
		-- the reverse moveVecA we have to move back.
		-- TODO: Determine how far we have to move back along reverse moveVecA
		reverseMoveVecA	= (-moveVecA)
		displacementX = 0.0
		displacementY = 0.0
