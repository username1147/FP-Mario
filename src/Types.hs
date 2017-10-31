{-# LANGUAGE DuplicateRecordFields #-}

module Types where


data Direction = Left | Up | Right | Down
	deriving (Eq, Show, Enum)

data Action = Action { moveDirection :: Direction, movementSpeed :: Float, actionStartTime :: Float }
	deriving (Eq, Show)



data Point = Point { x :: Double, y :: Double }
	deriving (Eq, Ord, Show)


data Coin = Coin Int
	deriving (Eq, Show)

data Mushroom = Mushroom
	deriving (Eq, Show)



data BlockDestructable = Destructable | Indestructable
	deriving (Eq, Show, Enum)


data Rectangle = Rectangle { topLeft :: Point, bottomRight :: Point}
	deriving (Eq, Show)



data Block = Block { pos :: Point, blockDestructable :: BlockDestructable, coin :: Maybe Coin }
	deriving (Eq, Show)

data FloorBlock = FloorBlock { pos :: Point }
	deriving (Eq, Show)

data Pipe = Pipe { pos :: Point }
	deriving (Eq, Show)

data ItemBlock = ItemBlock { pos :: Point, itemBlockDestructable :: BlockDestructable, mushroom :: Mushroom }
	deriving (Eq, Show)

data Enemy = Enemy { pos :: Point, enemyActions :: [Action] }
	deriving (Eq, Show)


-- This object is used to handle collisions
data Object = Object { pos :: Point, objectVelocity :: Point }
	deriving (Eq, Show)


-- Player objects can be either controlled by Player1, or by Player2
data PlayerControlType = Player1 | Player2
	deriving (Eq, Show, Enum)

data PlayerSize = Normal | Large
	deriving (Eq, Show, Enum)


data Player = Player { pos :: Point, controlledBy :: PlayerControlType, score :: Int, playerActions :: [Action], size :: PlayerSize }
	deriving (Eq, Show)


data Camera = Camera {
	pos :: Point,				-- Position of the camera in the world
	cameraWidth :: Int,			-- How wide (horizontal) the camera can see
	cameraHeight :: Int			-- How high (vertical) the camera can see
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





class Moveable a where
	getPosition :: a -> Point
	move :: a -> Action -> a


instance Moveable Block where
	getPosition = pos
	-- TODO: move

instance Moveable FloorBlock where
	getPosition = pos
	-- TODO: move

instance Moveable Pipe where
	getPosition = pos
	-- TODO: move

instance Moveable ItemBlock where
	getPosition = pos
	-- TODO: move

instance Moveable Enemy where
	getPosition = pos
	-- TODO: move

instance Moveable Object where
	getPosition = pos
	-- TODO: move

instance Moveable Player where
	getPosition = pos
	-- TODO: move

instance Moveable Camera where
	getPosition = pos
	-- TODO: move










