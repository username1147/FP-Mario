{-# language NamedFieldPuns #-}

module Controller where

import Graphics.Gloss.Interface.IO.Game
import Types
import Model
import Actions
import Rectangle
import Collision


--------------------------------------------------------------------------------
-- Some default Actions that are used for moving Mario
--------------------------------------------------------------------------------
moveLeft :: Float -> Action
moveLeft currentTime = Action {
	moveVector		= (-50.0, 0.0),
	actionStartTime	= currentTime
}

moveRight :: Float -> Action
moveRight currentTime = Action {
	moveVector		= (50.0, 0.0),
	actionStartTime	= currentTime
}

moveUp :: Float -> Action
moveUp currentTime = Action {
	moveVector		= (0.0, 50.0),
	actionStartTime	= currentTime
}

moveDown :: Float -> Action
moveDown currentTime = Action {
	moveVector		= (0.0, -50.0),
	actionStartTime	= currentTime
}

gravity :: Float -> Action
gravity currentTime = Action {
	moveVector		= (0.0, -9.81),
	actionStartTime	= currentTime
}



togglePause :: GameState -> GameState
togglePause gstate = gstate {paused = not (paused gstate)}



--------------------------------------------------------------------------------
-- Main functions for key input
--------------------------------------------------------------------------------

-- Move up, aka jump
inputKey :: Event -> GameState -> GameState -- Event is defined in the lib.
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		moveAction		= moveUp (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) moveAction

-- TODO: Remove moving up/jumping action if it exists...
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		moveAction		= moveUp (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) (reverseAction moveAction)



-- Move down
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		moveAction		= moveDown (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) moveAction

-- Stop moving down
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		moveAction		= moveDown (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) (reverseAction moveAction)



-- Move left
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
			}
	where
		playerObject	= player gstate
		moveAction		= moveLeft (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) moveAction

-- Stop moving left
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions =newAction }
			}
	where
		playerObject	= player gstate
		moveAction		= moveLeft (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) (reverseAction moveAction)



-- Move right
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
			}
	where
		playerObject	= player gstate
		moveAction		= moveRight (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) moveAction

-- Stop moving right
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
			}
	where
		playerObject	= player gstate
		moveAction		= moveRight (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) (reverseAction moveAction)


inputKey (EventKey (SpecialKey KeyEsc) _ _ _) _ = error "Escaped"
inputKey (EventKey (Char 'p') Down _ _) gstate = togglePause gstate
inputKey (EventKey (Char 'p') Up _ _) gstate = gstate
inputKey (EventKey (Char 'r') Up _ _) gstate = initialState $ resolution gstate -- r = revert back to init.
inputKey (EventKey (Char 'c') _ _ _) gstate = gstate {
	infoToShow = ShowNothing,
	camera = camera $ initialState $ resolution gstate,
	paused = paused gstate
}

inputKey _ gstate = initialState $ resolution gstate


input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- Returns all the static rectangles in the gamestate (e.g. blocks)
-- Does not return rectangles of players/enemies
getStaticRects :: GameState -> [Rectangle]
getStaticRects gstate = floorBlockRects ++ blockRects ++ pipeRects ++ itemBlockRects
	where
		levelMap			= level gstate
		blockRects			= map getRect $ blocks levelMap
		floorBlockRects		= map getRect $ floorBlocks levelMap
		itemBlockRects		= map getRect $ itemBlocks levelMap
		pipeRects			= map getRect $ pipes levelMap

-- Returns all the dynamic enemy rectangles in the gamestate
getEnemyRects :: GameState -> [Rectangle]
getEnemyRects gstate = map getRect $ enemies gstate

-- Handle collision of player with static rects... Ensures that after calling
-- this function, the player does not collide with static objects
-- TODO: FINISH HIM!
handleCollision :: GameState -> GameState
handleCollision gstate = handleCollisionHelper gstate 20


-- Helper function to handle collisions up until a certain depth, given by the
-- second Int parameter. Once that Int value reaches 0, maximum recursion depth
-- has been reached.
handleCollisionHelper :: GameState -> Int -> GameState
handleCollisionHelper gstate 0 = gstate		-- Maximum recursion depth reached
handleCollisionHelper gstate maxDepth
	| collisionStatic	= handleCollisionHelper newGameStateStatic (maxDepth - 1)
	| collisionEnemy	= handleCollisionHelper newGameStateEnemy (maxDepth - 1)
	| collisionEnemies	= handleCollisionHelper newGameStateEnemies (maxDepth - 1)
	| otherwise			= gstate	-- No collisions!
	where
		-- Check for player collision with the map
		playerObject		= player gstate
		currPlayerRect		= playerRect playerObject
		playerMoveVec		= deltaPositionVector (playerActions playerObject) (lastFrameTime gstate)
		staticRects			= getStaticRects gstate
		playerCollisions	= [(isCollision currPlayerRect rect, rect) | rect <- staticRects]
		collisionStatic		= any (\x -> fst x == True) playerCollisions

		-- If a collision happened, calculate displacement for the first one
		collisions			= filter (\x -> fst x == True) playerCollisions
		displacement
			| collisionStatic	= getCollisionDisplacement currPlayerRect firstCollisionRect playerMoveVec
			| otherwise			= (0.0, 0.0)
			where
				(_, firstCollisionRect)	= (!!) collisions 0

		-- Apply first displacement to new gamestate
		newPlayerRect		= shiftRectangle currPlayerRect displacement

		-- If a collision was handled, reset player movement
		newPlayerAction
			| collisionStatic	= defaultAction
			| otherwise			= playerActions playerObject

		newGameStateStatic	= gstate { player = playerObject {
									playerRect		= newPlayerRect,
									playerActions	= newPlayerAction } }

		-- TODO: Check for a collision between the player and an enemy
		collisionEnemy		= False
		newGameStateEnemy	= gstate

		-- TODO: Check for a collision between enemies and the map
		collisionEnemies	= False
		newGameStateEnemies	= gstate


step :: Float -> GameState -> IO GameState
step frameTime gstate
	| paused gstate 	= return $ gstate
	| otherwise 		= return $ newGameState
	where
		-- Update player positions, also add gravity
		playerObject	= player gstate
		playerAction	= addActions (gravity frameTime) (playerActions playerObject)
		playerShift		= deltaPositionVector playerAction frameTime
		newPlayerRect	= shiftRectangle (playerRect playerObject) playerShift

		-- TODO: Update enemy positions, also add gravity

		-- TODO: Check for player collision with enemies

		-- Check for collisions and handle them
		-- TODO: Also update enemy positions
		tempGameState	= gstate {
							lastFrameTime	= frameTime,
							player			= playerObject { playerRect = newPlayerRect }
						}
		safeGameState	= handleCollision tempGameState

		-- Update camera position based on new (safe) gamestate
		playerPos		= getCenter $ playerRect $ player safeGameState
		cameraObject	= camera gstate
		newCameraPos	= playerPos - convertToFloatTuple (resolutionHalf gstate)

		-- Update new game state
		newGameState	= safeGameState {
							lastFrameTime	= frameTime,
							elapsedTime		= elapsedTime gstate + frameTime,
							camera			= cameraObject { cameraPos = newCameraPos }
						}
