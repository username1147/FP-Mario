module Types where



data Direction = Left | Up | Right | Down
	deriving (Eq, Show, Enum)

data Action = Action {
	noveDirection :: Direction,		-- What direction this action will move
	movementSpeed :: Float,			-- How fast this action will move
	actionStartTime :: Float		-- At what (game)time the action was initiated
}


data ItemType = Coin | Mushroom
	deriving (Eq, Show, Enum)

data Item = Item {
	type :: ItemType,				-- What type of item
	duration :: Float,				-- How long the item will last when used
	itemUsedTime :: Float			-- When the item was used
}


-- Dynamic objects can move, Static objects don't move
data ObjectType = Static | Dynamic
	deriving (Eq, Show, Enum)

data StaticObjectType = NormalBlock | FloorBlock | Pipe | ItemBlock
	deriving (Eq, Show, Enum)


-- For dynamic objects, they can be either NPC controlled, or player controlled
data ObjectControlType = NPC | Player
	deriving (Eq, Show, Enum)

-- Player objects can be either controlled by Player1, or by Player2
data PlayerControlType = Player1 | Player2
	deriving (Eq, Show, Enum)

-- ObjectSize is only used for player-controlled objects, Big represents a
-- Mario after picking up a mushroom
data ObjectSize = Normal | Big
	deriving (Eq, Show, Enum)

-- Positions are respresented as 2D coordinates using an x and y for horizontal
-- and vertical positioning respectively
data Position = Position {
	x :: Float,
	y :: Float
}


data Object = Object {
	type :: ObjectType,				-- If its static or dynamic
	breakable :: Maybe Bool,		-- If its a static object, is it breakable or not?
	position :: Position,			-- Object position
	controlType :: Maybe ObjectControlType,		-- If its a dynamic object, who controls it?
	playerControl :: Maybe PlayerControlType,	-- If its a player object, which player controls it?
	playerScore :: Maybe Integer,				-- If its a player object, what is his score?
	items :: [Item],				-- List of items the object has or can drop
	actions :: Maybe [Action],		-- For dynamic objects, what actions the object does
	size :: Maybe ObjectSize		-- For player objects, what is the player size
}


data Camera = Camera {
	position :: Position,			-- Position of the camera in the world
	cameraWidth :: Integer,			-- How wide (horizontal) the camera can see
	cameraHeight :: Integer			-- How high (vertical) the camera can see
}


data GameState = GameState {
	objects :: [Object],			-- All the objects in the game
	camera :: Camera				-- A camera to view the world
}
