module Types where



data Direction = Left | Up | Right | Down
	deriving (Eq, Show, Enum)

data Action = Action {
	noveDirection :: Direction,		-- What direction this action will move
	movementSpeed :: Float,			-- How fast this action will move
	actionStartTime :: Float		-- At what (game)time the action was initiated
}



data Coin = Coin Int
	deriving (Eq, Show)

data Mushroom = Mushroom
	deriving (Eq, Show)



data BlockDestructable = Destructable | Indestructable
	deriving (Eq, Show, Enum)


data Rectangle = Rectangle Point Point
	deriving (Eq, Show)



data Block = Block { position :: Point, destructable :: BlockDestructable, coin :: Maybe Coin }
	deriving (Eq, Show)

data FloorBlock = FloorBlock { position :: Point }
	deriving (Eq, Show)

data Pipe = Pipe { position :: Point }
	deriving (Eq, Show)

data ItemBlock = ItemBlock { position :: Point, destructable :: BlockDestructable, mushroom :: Mushroom }
	deriving (Eq, Show)



data Enemy = Enemy { position :: Point, actions :: [Action] }
	deriving (Eq, Show)



-- Player objects can be either controlled by Player1, or by Player2
data PlayerControlType = Player1 | Player2
	deriving (Eq, Show, Enum)

data PlayerSize = Normal | Large
	deriving (Eq, Show, Enum)


data Player = Player { position :: Point, controlledBy :: PlayerControlType, score :: Int, actions :: [Action], size :: PlayerSize }
	deriving (Eq, Show)


data Camera = Camera {
	position :: Point,				-- Position of the camera in the world
	cameraWidth :: Integer,			-- How wide (horizontal) the camera can see
	cameraHeight :: Integer			-- How high (vertical) the camera can see
}


data GameState = GameState {
	blocks :: [Block],
	floorBlocks :: [FloorBlock],
	pipes :: [Pipe],
	itemBlocks :: [ItemBlock],
	player :: Player,
	enemies :: [Enemy],
	camera :: Camera
}
