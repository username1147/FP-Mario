{-# language NamedFieldPuns #-}

module Controller where

import Graphics.Gloss.Interface.IO.Game
import Types
import Model
import Vector
import Actions
import Rectangle
import Collision


--------------------------------------------------------------------------------
-- Some default Actions that are used for moving Mario
--------------------------------------------------------------------------------
moveLeft :: Float -> Action
moveLeft currentTime = Action {
	moveVector		= (-150.0, 0.0),
	actionStartTime	= currentTime
}

moveRight :: Float -> Action
moveRight currentTime = Action {
	moveVector		= (150.0, 0.0),
	actionStartTime	= currentTime
}

moveUp :: Float -> Action
moveUp currentTime = Action {
	moveVector		= (0.0, 500.0),
	actionStartTime	= currentTime
}

moveDown :: Float -> Action
moveDown currentTime = Action {
	moveVector		= (0.0, -50.0),
	actionStartTime	= currentTime
}


-- Given a certain (current) action, returns the new action such that the player
-- is no longer jumper up
stopMovingUp :: Action -> Action
stopMovingUp (Action (x, _) time) = Action { moveVector = (x, 0.0), actionStartTime = time }


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
		-- newAction		= addActions (playerActions playerObject) (reverseAction moveAction)
		newAction		= stopMovingUp $ playerActions playerObject



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

inputKey _ gstate = gstate


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



-- Data type that is only used for the handle collision helper function
data CollisionType = NormalCollision | GravityCollision
	deriving (Eq, Enum, Show)

-- Handle collision of player with static rects... Ensures that after calling
-- this function, the player does not collide with static objects
handleCollision :: GameState -> GameState
handleCollision gstate = handleCollisionHelper NormalCollision gstate (20 * numEnemies)
	where
		numEnemies = 1 + (length $ enemies gstate)

-- Similar to handleCollision, except for gravity
handleCollisionGravity :: GameState -> GameState
handleCollisionGravity gstate = handleCollisionHelper GravityCollision gstate (20 * numEnemies)
	where
		numEnemies = 1 + (length $ enemies gstate)


-- Helper function to handle collisions up until a certain recursion depth,
-- given by the second Int parameter. Once that Int value reaches 0, maximum
-- recursion depth has been reached and the last adapted gamestate will be
-- returned. The second argument CollisionType determines what action will
-- be used to get the moveVector of the player (normal or gravity)
handleCollisionHelper :: CollisionType -> GameState -> Int -> GameState
handleCollisionHelper _ gstate 0 = gstate		-- Maximum recursion depth reached
handleCollisionHelper colisionType gstate maxDepth
	| collisionStatic					= handleCollisionHelper colisionType newGameStateStatic (maxDepth - 1)
	| any (== True) collisionEnemy		= handleCollisionHelper colisionType newGameStateEnemy (maxDepth - 1)
	| any (== True) collisionEnemies	= handleCollisionHelper colisionType newGameStateEnemies (maxDepth - 1)
	| otherwise							= gstate	-- No collisions!
	where
		-- Check for player collision with the map
		playerObject		= player gstate
		currPlayerRect		= playerRect playerObject
		playerMoveVec
			| colisionType == NormalCollision	= deltaPositionVector (playerActions playerObject) (lastFrameTime gstate)
			| colisionType == GravityCollision	= deltaPositionVector (playerGravity playerObject) (lastFrameTime gstate)

		staticRects			= getStaticRects gstate
		playerCollisions	= [(isCollision currPlayerRect rect, rect) | rect <- staticRects]
		lambdaFunc			= (\x -> fst x == True)
		collisionStatic		= any lambdaFunc playerCollisions

		-- If a collision happened, calculate displacement for the first one
		collisions			= filter lambdaFunc playerCollisions
		displacement
			| collisionStatic	= getCollisionDisplacement currPlayerRect firstCollisionRect playerMoveVec
			| otherwise			= (0.0, 0.0)
			where
				firstCollisionRect = snd $ head collisions

		-- Apply first displacement to new gamestate
		newPlayerRect		= shiftRectangle currPlayerRect displacement

		-- If a collision was handled, reset player gravity. NOT the player actions!
		newPlayerGravity
			| collisionStatic	= defaultAction 	-- Not defaultGravityAction,
													-- because that is added
													-- in the step function!
			| otherwise			= playerGravity playerObject

		newGameStateStatic	= gstate { player = playerObject {
									playerRect		= newPlayerRect,
									playerGravity	= newPlayerGravity } }

		-- TODO: Check for a collision between the player and an enemy
		collisionEnemy		= [False]
		newGameStateEnemy	= gstate

		-- Check for a collision between enemies and the map
		enemyObjects		= enemies gstate
		numEnemies			= length enemyObjects
		currEnemyRects		= map getRect enemyObjects
		currEnemyDeathRects	= map deathRect enemyObjects
		enemyCollisions		= [[(isCollision enemyRect rect, rect) | rect <- staticRects] | enemyRect <- currEnemyRects]
		collisionEnemies	= [any lambdaFunc enemyCollision | enemyCollision <- enemyCollisions]
		enemyMoveVecs
			| colisionType == NormalCollision	= [deltaPositionVector (enemyActions enemyObject) (lastFrameTime gstate) | enemyObject <- enemyObjects]
			| colisionType == GravityCollision	= [deltaPositionVector (enemyGravity enemyObject) (lastFrameTime gstate) | enemyObject <- enemyObjects]

		-- If a collision happened, calculate displacement for the first one of each enemy
		collisionsEnemy		= [filter lambdaFunc enemyCollision | enemyCollision <- enemyCollisions]
		crapRectangle		= Rectangle (0.0, 0.0) (0.0, 0.0)
		firstCollisionRects	= [if (length collisionEnemy > 0)
									then snd $ head collisionEnemy
									else crapRectangle
								| collisionEnemy
								<- collisionsEnemy]
		displacements		 = [if (firstCollisionRect /= crapRectangle)
									then getCollisionDisplacement currEnemyRect firstCollisionRect enemyMoveVec
									else (0.0, 0.0)
								| (currEnemyRect, firstCollisionRect, enemyMoveVec)
								<- zip3 currEnemyRects firstCollisionRects enemyMoveVecs]

		-- If a collision was handled, reset player gravity. NOT the player actions!
		newEnemiesGravity	= [if (displacement /= (0.0, 0.0))
									then enemyGravity enemObject
									else defaultAction
								| (enemObject, displacement)
								<- zip enemyObjects displacements]
		newEnemyObjects		= [
			Enemy {
				enemyRect		= shiftRectangle (enemyRect enemyObject) enemyDisplacement,
				deathRect		= shiftRectangle (deathRect enemyObject) enemyDisplacement,
				enemyActions	= enemyActions enemyObject,
				enemyGravity	= newGravity
			}
			| (enemyObject, enemyDisplacement, newGravity)
			<- zip3 enemyObjects displacements newEnemiesGravity]

		newGameStateEnemies	= gstate { enemies = newEnemyObjects }

		-- TODO: Check for a collision between enemies individually?


step :: Float -> GameState -> IO GameState
step frameTime gstate
	| paused gstate 	= do
							return $ gstate
	| otherwise 		= do
							outputFile <- return ("src/output.txt" :: FilePath)

							-- Write only once every second...
							if convertToInt (elapsedTime gstate + frameTime) > convertToInt (elapsedTime gstate)
								then appendFile outputFile ("Time elapsed:" ++ show (elapsedTime gstate) ++ "\n")
								else return ()
							return $ newGameState
	where
		-- Update player positions without gravity
		playerObject	= player gstate
		playerAction	= playerActions playerObject
		playerShift		= deltaPositionVector playerAction frameTime
		newPlayerRect	= shiftRectangle (playerRect playerObject) playerShift

		-- Update enemy positions without gravity
		enemyObjects	= enemies gstate
		enemyActionList	= map enemyActions enemyObjects
		enemyShifts		= [deltaPositionVector enemyAction frameTime | enemyAction <- enemyActionList]
		enemiesZipped	= zip enemyObjects enemyShifts
		newEnemyRects	= [shiftRectangle (enemyRect enemyObject) enemyShift | (enemyObject, enemyShift) <- enemiesZipped]
		newDeathRects	= [shiftRectangle (deathRect enemyObject) enemyShift | (enemyObject, enemyShift) <- enemiesZipped]
		newObjects		= [enemyObject { enemyRect = newEnemyRect, deathRect = newDeathRect } | (enemyObject, newEnemyRect, newDeathRect) <- zip3 enemyObjects newEnemyRects newDeathRects]

		-- Super ASI mode enabled with ultra intelligence: move towards player
		playerCenter	= getCenter $ getRect playerObject
		(playerX, _)	= playerCenter
		xDifferences	= map (\enemy -> playerX - (getCenterX $ getRect enemy)) enemyObjects
		xMovements		= [if x >= 0.0 then 50.0 else -50.0 | x <- xDifferences]
		newEnemyObjects	= [enemObject { enemyActions = Action { moveVector = (xMove, 0.0), actionStartTime = 0.0 } }
							| (enemObject, xMove)
							<- zip newObjects xMovements]

		-- Check for collisions and handle them
		tempGameState	= gstate {
							lastFrameTime	= frameTime,
							player			= playerObject { playerRect = newPlayerRect },
							enemies			= newEnemyObjects
						}
		tempGameState2	= handleCollision tempGameState

		-- Add (more) gravity to player, to simulate falling down faster
		gravityPlayer 			= addActions (playerGravity playerObject) defaultGravityAction
		playerShiftGravity		= deltaPositionVector gravityPlayer frameTime
		playerObject2			= player tempGameState2
		newPlayerRect2			= playerRect $ playerObject2
		newPlayerRectGravity	= shiftRectangle newPlayerRect2 playerShiftGravity

		-- Add (more) gravity to enemies, to simulate falling down faster
		gravityEnemies			= [addActions (enemyGravity enemyObject) defaultGravityAction | enemyObject <- enemyObjects]
		enemyShiftsGravity		= [deltaPositionVector gravityEnemy frameTime | gravityEnemy <- gravityEnemies]
		enemyObjects2			= enemies tempGameState2
		enemiesZipped2			= zip3 enemyObjects2 enemyShiftsGravity gravityEnemies
		newEnemyObjects2		= [
			Enemy {
				enemyRect		= shiftRectangle (enemyRect enemyObject) gravityShift,
				deathRect		= shiftRectangle (deathRect enemyObject) gravityShift,
				enemyActions	= enemyActions enemyObject,
				enemyGravity	= newGravity
			} | (enemyObject, gravityShift, newGravity) <- enemiesZipped2]

		-- Again, check for collisions and handle them
		tempGameState3	= tempGameState2 {
							player = playerObject2 {
								playerRect		= newPlayerRectGravity,
								playerGravity	= gravityPlayer
							},
							enemies = newEnemyObjects2
						}
		safeGameState	= handleCollisionGravity tempGameState3

		-- Update camera position based on new (safe) gamestate
		playerPos		= getCenter $ playerRect $ player safeGameState
		cameraObject	= camera safeGameState
		newCameraPos	= playerPos - convertToFloatTuple (resolutionHalf safeGameState)

		-- Update new game state
		newGameState	= safeGameState {
							lastFrameTime	= frameTime,
							elapsedTime		= elapsedTime safeGameState + frameTime,
							camera			= cameraObject { cameraPos = newCameraPos }
						}
