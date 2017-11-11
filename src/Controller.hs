{-# language NamedFieldPuns #-}

module Controller where

import Graphics.Gloss.Interface.IO.Game
import Types
import Model
import Actions
import Rectangle


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



-- Supplementary function for inputKey
shiftCameraWithMario :: Float -> Float -> Camera -> Camera
shiftCameraWithMario x y cam = cam { cameraPos = cameraPos cam + (x, y)}

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
				-- TODO: Move camera position updating to updateGameState function
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (0, -10) }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		cameraObject	= camera gstate
		moveAction		= moveUp (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) moveAction

-- TODO: Remove moving up/jumping action if it exists...
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
				-- TODO: Move camera position updating to updateGameState function
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (0, -10) }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		cameraObject	= camera gstate
		moveAction		= moveUp (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) (reverseAction moveAction)



-- Move down
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
				-- TODO: Move camera position updating to updateGameState function
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (0, 10) }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		cameraObject	= camera gstate
		moveAction		= moveDown (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) moveAction

-- Stop moving down
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
				-- TODO: Move camera position updating to updateGameState function
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (0, 10) }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		cameraObject	= camera gstate
		moveAction		= moveDown (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) (reverseAction moveAction)



-- Move left
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
				-- TODO: Move camera position updating to updateGameState function
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (10, 0) }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		cameraObject	= camera gstate
		moveAction		= moveLeft (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) moveAction

-- Stop moving left
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions =newAction }
				-- TODO: Move camera position updating to updateGameState function
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (10, 0) }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		cameraObject	= camera gstate
		moveAction		= moveLeft (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) (reverseAction moveAction)



-- Move right
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
				-- TODO: Move camera position updating to updateGameState function
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (10, 0) }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		cameraObject	= camera gstate
		moveAction		= moveRight (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) moveAction

-- Stop moving right
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerActions = newAction }
				-- TODO: Move camera position updating to updateGameState function
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (10, 0) }
			}
	where
		-- TODO: Check if we're already jumping up and act accordingly!
		playerObject	= player gstate
		cameraObject	= camera gstate
		moveAction		= moveRight (elapsedTime gstate)
		newAction		= addActions (playerActions playerObject) (reverseAction moveAction)


inputKey (EventKey (SpecialKey KeyEsc) _ _ _) _ = error "Escaped"
inputKey (EventKey (Char 'p') Down _ _) gstate = togglePause gstate
inputKey (EventKey (Char 'p') Up _ _) gstate = gstate
inputKey (EventKey (Char 'r') Up _ _) gstate = initialState -- r = revert back to init.
inputKey (EventKey (Char 'c') _ _ _) gstate = gstate {
	infoToShow = ShowNothing,
	camera = camera initialState,
	paused = False
}

inputKey _ gstate = initialState

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

step :: Float -> GameState -> IO GameState
step frameTime gstate = return $ newGameState
	where
		-- Update player positions, also add gravity
		playerObject	= player gstate
		playerAction	= addActions (gravity frameTime) (playerActions playerObject)
		playerShift		= actionMovementVector playerAction frameTime
		newPlayerRect	= shiftRectangle (playerRect playerObject) playerShift

		-- Update camera position
		cameraObject	= camera gstate
		newCameraPos	= getCenter newPlayerRect - (200, 200)	-- Screen resolution...

		-- Update new game state
		newGameState	= gstate {
							lastFrameTime	= frameTime,
							elapsedTime		= elapsedTime gstate + frameTime,
							player 			= playerObject { playerRect = newPlayerRect },
							camera			= cameraObject { cameraPos = newCameraPos }
						}
