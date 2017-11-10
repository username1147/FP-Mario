
module Actions where

import Graphics.Gloss.Data.Point

data Action = Action { directionVector :: Point, verticalMovementSpeed :: Float, horizontalMovementSpeed :: Float, actionStartTime :: Float }
	deriving (Eq, Show)

-- Returns a movement vector from the given action, the
-- TODO: Add gravity...?
actionMovementVector :: Action -> Float -> Point
actionMovementVector (Action (x, y) v h actionTime) deltaTime = (x * h * deltaTime, y * v * deltaTime)


-- Returns a default Action that does nothing
defaultAction :: Action
defaultAction = Action {
	directionVector			= (0.0, 0.0),
	verticalMovementSpeed	= 0.0,
	horizontalMovementSpeed	= 0.0,
	actionStartTime			= 0.0
}

-- Returns the "opposite"/reverse action
reverseAction :: Action -> Action
reverseAction (Action dirVec vertSpeed horSpeed actTime) = Action {
	directionVector			= -dirVec,
	verticalMovementSpeed	= -vertSpeed,
	horizontalMovementSpeed	= -horSpeed,
	actionStartTime			= -actTime
}


-- TODO: Combine 2 actions into 1
addActions :: Action -> Action -> Action
addActions action1 action2 = Action {
	directionVector			= dirVec1 + dirVec2,
	verticalMovementSpeed	= vertSpeed1 + vertSpeed2,
	horizontalMovementSpeed	= horSpeed1 + horSpeed2,
	actionStartTime			= max actTime1 actTime2 }
	where
		dirVec1		= directionVector action1
		dirVec2		= directionVector action2
		vertSpeed1	= verticalMovementSpeed action1
		vertSpeed2	= verticalMovementSpeed action2
		horSpeed1	= horizontalMovementSpeed action1
		horSpeed2	= horizontalMovementSpeed action2
		actTime1	= actionStartTime action1
		actTime2	= actionStartTime action2




-- TODO: Finish this or rewrite it, whatever
-- applyActions :: Moveable a => Actionable a => Rectangleable a => a -> a
-- applyActions object
-- 	| isJust leftRight	=
-- 	where
-- 		objectRect	= getRect object
-- 		actions		= getActions object
-- 		leftRight	= leftOrRight actions
-- 		upDown		= upOrDown actions
