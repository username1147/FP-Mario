module Types where




data Direction = Left | Up | Right | Down
	deriving (Eq, Show, Enum)

data Action = Action { moveDirection :: Direction, movementSpeed :: Float, actionStartTime :: Float }
	deriving (Eq, Show)



data Point = Point { x :: Double, y :: Double }
	deriving (Eq, Show)



class Moveable a where
	getPosition :: a -> Point
	move :: a -> Action -> a



data Coin = Coin Int
	deriving (Eq, Show)

data Mushroom = Mushroom
	deriving (Eq, Show)



data BlockDestructable = Destructable | Indestructable
	deriving (Eq, Show, Enum)


data Rectangle = Rectangle { topLeft :: Point, bottomRight :: Point}
	deriving (Eq, Show)



data Block = Block { blockPos :: Point, blockDestructable :: BlockDestructable, coin :: Maybe Coin }
	deriving (Eq, Show)

data FloorBlock = FloorBlock { floorBlockPos :: Point }
	deriving (Eq, Show)

data Pipe = Pipe { pipePos :: Point }
	deriving (Eq, Show)

data ItemBlock = ItemBlock { itemBockPos :: Point, itemBlockDestructable :: BlockDestructable, mushroom :: Mushroom }
	deriving (Eq, Show)



data Enemy = Enemy { enemyPos :: Point, enemyActions :: [Action] }
	deriving (Eq, Show)



-- Player objects can be either controlled by Player1, or by Player2
data PlayerControlType = Player1 | Player2
	deriving (Eq, Show, Enum)

data PlayerSize = Normal | Large
	deriving (Eq, Show, Enum)


data Player = Player { playerPos :: Point, controlledBy :: PlayerControlType, score :: Int, playerActions :: [Action], size :: PlayerSize }
	deriving (Eq, Show)


data Camera = Camera {
	position :: Point,			-- Position of the camera in the world
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
