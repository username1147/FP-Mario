module Vector where

import Graphics.Gloss.Data.Point



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
