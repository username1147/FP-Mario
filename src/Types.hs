{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Graphics.Gloss.Data.Point
import Rectangle
import Actions

--------------------------------------------------------------------------------
-- Data types for game code
--------------------------------------------------------------------------------

-- Screen resolution type alias
type Resolution = (Int, Int)

convertToInt :: Float -> Int
convertToInt x = truncate x

convertToFloat :: Int -> Float
convertToFloat x = fromIntegral x

multiplyTuple :: Num a => (a, a) -> a -> (a, a)
multiplyTuple (x, y) scale = (x * scale, y * scale)

convertToIntTuple :: (Float, Float) -> (Int, Int)
convertToIntTuple (x, y) = (convertToInt x, convertToInt y)

convertToFloatTuple :: (Int, Int) -> (Float, Float)
convertToFloatTuple (x, y) = (convertToFloat x, convertToFloat y)


--------------------------------------------------------------------------------
-- General data types
--------------------------------------------------------------------------------

data Coin = Coin Int
	deriving (Eq, Show)

data Mushroom = Mushroom
	deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Data types with regards to blocks
--------------------------------------------------------------------------------

data BlockDestructable = Destructable | Indestructable
	deriving (Eq, Show, Enum)


data Block = Block { blockRect :: Rectangle, blockDestructable :: BlockDestructable, coin :: Maybe Coin }
	deriving (Eq, Show)

data FloorBlock = FloorBlock { floorBlockRect :: Rectangle }
	deriving (Eq, Show)

data Pipe = Pipe { pipeRect :: Rectangle }
	deriving (Eq, Show)

data ItemBlock = ItemBlock { itemBlockRect :: Rectangle, itemBlockDestructable :: BlockDestructable, mushroom :: Mushroom }
	deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Data types with regards to enemies and players
--------------------------------------------------------------------------------

data Enemy = Enemy { enemyRect :: Rectangle, enemyActions :: Action }
	deriving (Eq, Show)


-- Player objects can be either controlled by Player1, or by Player2
data PlayerControlType = Player1 | Player2
	deriving (Eq, Show, Enum)

data PlayerSize = Normal | Large
	deriving (Eq, Show, Enum)


data Player = Player {
	playerRect :: Rectangle,
	controlledBy :: PlayerControlType,
	score :: Int,
	playerActions :: Action,
	size :: PlayerSize
} deriving (Eq, Show)



--------------------------------------------------------------------------------
-- Data types for the camera, levels and gamestate
--------------------------------------------------------------------------------

data Camera = Camera {
	cameraPos :: Point,			-- Position of the camera in the world
	cameraWidth :: Int,			-- How wide (horizontal) the camera can see
	cameraHeight :: Int			-- How high (vertical) the camera can see
} deriving (Eq, Show)


-- TODO: Remove this and actually use a list of blocks etc...
data InfoToShow = ShowNothing | ShowGame
	deriving (Eq, Show)

data LevelMap = LevelMap {
	blocks :: [Block],
	floorBlocks :: [FloorBlock],
	pipes :: [Pipe],
	itemBlocks :: [ItemBlock],
	mapHeight :: Int,
	mapLength :: Int
} deriving (Eq, Show)

data GameState = GameState {
	infoToShow :: InfoToShow,
	level :: LevelMap,
	player :: Player,
	enemies :: [Enemy],
	camera :: Camera,
	resolution :: Resolution,
	resolutionHalf :: Resolution,
	lastFrameTime :: Float,
	elapsedTime :: Float,
	paused :: Bool
} deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Class and instances for objects that are "defined as a Rectangle"
--------------------------------------------------------------------------------
class Rectangleable a where
	getRect :: a -> Rectangle


instance Rectangleable Block where
	getRect = blockRect

instance Rectangleable FloorBlock where
	getRect = floorBlockRect

instance Rectangleable Pipe where
	getRect = pipeRect

instance Rectangleable ItemBlock where
	getRect = itemBlockRect

instance Rectangleable Enemy where
	getRect = enemyRect

instance Rectangleable Player where
	getRect = playerRect


--------------------------------------------------------------------------------
-- Class and instances for objects that can perform actions
--------------------------------------------------------------------------------
class Actionable a where
	getActions :: a -> Action


instance Actionable Enemy where
	getActions = enemyActions

instance Actionable Player where
	getActions = playerActions



--------------------------------------------------------------------------------
-- Class and instances for Moveable objects
--------------------------------------------------------------------------------

class Moveable a where
	getPosition :: a -> Point
	move :: a -> Action -> a


instance Moveable Block where
	getPosition b = bottomLeft $ blockRect b
	move b _ = b -- blocks are not supposed to move

instance Moveable FloorBlock where
	getPosition f = bottomLeft $ floorBlockRect f
	move f _ = f -- floor blocks don't move

instance Moveable Pipe where
	getPosition p = bottomLeft $ pipeRect p
	move p _ = p -- pipes don't move

instance Moveable ItemBlock where
	getPosition i = bottomLeft $ itemBlockRect i
	move i _ = i

-- instance Moveable Enemy where
-- 	getPosition e = bottomLeft $ enemyRect e

-- 	move en act = en { -- move function
-- 			enemyRect = shiftRectangle (getRect en) (actionMovementVector act),
-- 			enemyActions = Action (0, 0) 0 0 (actionStartTime act) -- what to do with actionTime?
-- 		}

-- instance Moveable Player where
-- 	getPosition pl = bottomLeft $ playerRect pl
-- 	move pl act = pl {
-- 			playerRect = shiftRectangle (getRect pl) (actionMovementVector act),
-- 			playerActions = Action (0, 0) 0 0 (actionStartTime act)
-- 		}





