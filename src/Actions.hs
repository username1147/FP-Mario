module Actions where

import Graphics.Gloss.Data.Point

-- The action datatype actually contains a moveVector that represents the
-- current horizontal and vertical speeds for this action.
data Action = Action { moveVector :: Point, actionStartTime :: Float }
	deriving (Eq, Show)


-- Returns a displacement vector from the given action, given a certain deltaTime
deltaPositionVector :: Action -> Float -> Point
deltaPositionVector (Action (vx, vy) actionTime) deltaTime = (vx * deltaTime, vy * deltaTime)


-- Returns a default Action that does nothing
defaultAction :: Action
defaultAction = Action {
	moveVector		= (0.0, 0.0),
	actionStartTime	= 0.0
}


-- Returns the "opposite"/reverse action
reverseAction :: Action -> Action
reverseAction (Action moveVec actTime) = Action {
	moveVector		= -moveVec,
	actionStartTime	= -actTime
}


-- Combine 2 actions into 1, by adding individual fields
addActions :: Action -> Action -> Action
addActions action1 action2 = Action {
	moveVector		= moveVec1 + moveVec2,
	actionStartTime	= min actTime1 actTime2 }
	where
		moveVec1	= moveVector action1
		moveVec2	= moveVector action2
		actTime1	= actionStartTime action1
		actTime2	= actionStartTime action2
