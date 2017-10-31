module Model where

import Graphics.Gloss.Data.Point

import Types


absToRelCoord :: Point -> Point -> Point -- abs. pos -> screen abs. pos.
absToRelCoord p1 p2 = p1 + p2

-- TODO: Add adjustable screen resolution
levelToScreenCoord :: Point -> Point -- because left-bottom of level has coord. (0, 0)
-- for the screen it is (-width/2, -height/2)
levelToScreenCoord (x, y) = (x - 200, y - 200) -- assuming screen is 400x400


-- now create some blocks for a sample level
block1 = FloorBlock {
	floorBlockRect = Rectangle {
		topLeft         = (0.0,		50.0),
		bottomRight     = (500.0,	0.0)
	}
}

block2 = Block {
	blockRect = Rectangle {
		topLeft         = (300,		400),
		bottomRight     = (800,		350)
	},
	blockDestructable = Destructable,
	coin = Just (Coin 100)
}

sampleLevel :: LevelMap
sampleLevel = LevelMap {
	blocks = [block2],
	floorBlocks = [block1],
	pipes = [],
	itemBlocks = [],
	mapHeight = 400,
	mapLength = 800
}

initialState :: GameState
initialState = GameState {
	infoToShow = ShowGame,
	player = Player {
		playerRect = Rectangle {
			topLeft         = (0.0,		10.0),
			bottomRight     = (10.0,	0.0)
		},
		controlledBy = Player1,
		score = 0,
		playerActions = [],
		size = Normal
	},
	enemies = [],
	camera = Camera {
		pos = (0, 0),
		cameraWidth = 400,
		cameraHeight = 400
	},
	level = sampleLevel,
	elapsedTime = 0.0
}

nO_SECS_BETWEEN_CYCLES :: Float -- what is that for???
nO_SECS_BETWEEN_CYCLES = 5
