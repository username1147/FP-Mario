module Vector where

import Graphics.Gloss.Data.Point


-- Epsilon value that can be used to compensate for floating-point precision
-- errors
epsilon :: Float
epsilon = 0.01



-- Returns the dot product between 2 vectors/points
dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- Returns the subtraction of 2 vectors/points
sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Scales the given vector/point with the given value
scale :: Point -> Float -> Point
scale (x, y) s = (x * s, y * s)

-- Returns the squared length of the vector
vectorLengthSquared :: Point -> Float
vectorLengthSquared x = dot x x

-- Returns the length of the vector
vectorLength :: Point -> Float
vectorLength x = sqrt $ vectorLengthSquared x

-- Returns the projection of vector A on vector B
projection :: Point -> Point -> Point
projection pA pB = scale pA projectionLength
	where
		projectionLength = (dot pA pB) / (vectorLengthSquared pA)

-- Returns the reverse vector (aka -vector)
reverseVector :: Point -> Point
reverseVector (x, y) = (-x, -y)
