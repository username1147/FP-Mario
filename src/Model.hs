module Model where

import Graphics.Gloss.Data.Point

import Types
import Actions
import Rectangle



-- Now create a sample level
sampleLevel :: LevelMap
sampleLevel = LevelMap {
	blocks = [Block {
		blockRect = Rectangle {
			bottomLeft	= (300,	350),
			topRight	= (800,	400)
		},
		blockDestructable = Destructable,
		coin = Just (Coin 100)
	}],
	floorBlocks = [FloorBlock {
		floorBlockRect = Rectangle {
			bottomLeft	= (0.0,		0.0),
			topRight	= (500.0,	50.0)
		}
	}],
	pipes = [],
	itemBlocks = [],
	mapHeight = 400,
	mapLength = 800
}


-- The initial state is just the sample level with a default player position
initialState :: Resolution -> GameState
initialState windowResolution@(resolutionWidth, resolutionHeight) = GameState {
	infoToShow = ShowGame,
	player = Player {
		playerRect = playerStartRect,
		controlledBy = Player1,
		score = 0,
		playerActions = defaultAction,
		playerGravity = defaultGravityAction,
		size = Normal
	},
	enemies = [],
	camera = Camera {
		cameraPos = getCenter playerStartRect - (halfWidth, halfHeight),
		cameraWidth = resolutionWidth,
		cameraHeight = resolutionHeight
	},
	resolution = windowResolution,
	resolutionHalf = halfResolution,
	level = sampleLevel,
	lastFrameTime = 0.0,
	elapsedTime = 0.0,
	paused = False }
	where
		-- The player starting rect is defined here so we can focus the camera
		-- on it when we start the level
		playerStartRect			= Rectangle {
									bottomLeft	= (30.0, 100.0),
									topRight	= (40.0, 110.0)
								}
		floatResolution 		= convertToFloatTuple windowResolution
		halfResolution			= convertToIntTuple $ multiplyTuple floatResolution 0.5
		(halfWidth, halfHeight)	= convertToFloatTuple halfResolution

