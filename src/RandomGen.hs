module RandomGen where

-- This module is used to produce custom random objects
-- The output type is IO a, because these functions will
-- be used in Main's do-block

import Test.QuickCheck

import Actions
import Rectangle
import Types
import Model

{-
-- Test function
generateProduct :: IO Int
generateProduct = do
					x <- generate (choose (1, 10) :: Gen Int)
					y <- generate (choose (-10, -1) :: Gen Int)
					return (x * y)

-- Add parameters here
generateRectangle :: IO Rectangle
generateRectangle = do
						x1 <- generate (choose (0, 100) :: Gen Float)
						y1 <- generate (choose (0, 100) :: Gen Float)
						x2 <- generate (choose (101, 200) :: Gen Float)
						y2 <- generate (choose (101, 200) :: Gen Float)
						return (Rectangle {bottomLeft = (x1, y1), topRight = (x2, y2)})

-- possibly add parameters here
generateEnemy :: IO Enemy
generateEnemy = do
					rect <- generateRectangle
					return (Enemy {enemyRect = rect, enemyActions = defaultAction})

generateBlock :: IO Block
generateBlock = do
					rect	<- generateRectangle
					dest	<- generate (elements [Destructable, Indestructable])
					c		<- generate (choose (1, 10) :: Gen Int)
					cn		<- generate (elements [Nothing, Just (Coin c)])
					return (Block {blockRect = rect, blockDestructable = dest, coin = cn})

generateFloorBlock :: IO FloorBlock
generateFloorBlock = do
						rect <- generateRectangle
						return (FloorBlock rect)

generatePipe :: IO Pipe
generatePipe = do
					rect <- generateRectangle
					return (Pipe rect)

generateItemBlock :: IO ItemBlock
generateItemBlock = do
						rect <- generateRectangle
						dest <- generate (elements [Destructable, Indestructable])
						return (ItemBlock rect dest Mushroom)
-}

-- assuming that the size of enemy is 50x50
spawnEnemy :: FloorBlock -> Enemy
spawnEnemy fb@(FloorBlock {floorBlockRect = Rectangle (xl, yl) (xr, yh)}) =
    Enemy {
        enemyRect = Rectangle ((xl + xr)/2 - 25, yh + 50) ((xl + xr)/2 + 25, yh + 100),
        enemyActions = defaultAction
    }

spawnEnemies :: GameState -> GameState
spawnEnemies gstate = gstate {enemies = map spawnEnemy fbs}
    where fbs = floorBlocks $ level $ gstate

-- thinkness of a block is assumed to be 50; the first block starts at (0, 0)
createListOfFloorBlocks :: Int -> [Float] -> [Float] -> [Float] -> Float -> [FloorBlock]
createListOfFloorBlocks 0 _ _ _ _					= []
createListOfFloorBlocks _ [] _ _ _					= []
createListOfFloorBlocks _ _ [] _ _					= []
createListOfFloorBlocks _ _ _ [] _					= []
createListOfFloorBlocks n (d:ds) (l:ls) (h:hs) ref	= FloorBlock (Rectangle (ref,h) (ref + l,h + 50))
							: (createListOfFloorBlocks (n-1) ds ls hs (ref + d + l))

generateLevelFloorBlocks :: Int -> Float -> IO [FloorBlock]
generateLevelFloorBlocks n mDist = do
    dists <- generate (suchThat (arbitrary :: Gen [Float]) (\x -> (length x == n) && (maximum x <= mDist)))
    dists <- return (dists `add` replicate (length dists) ((-1)*(minimum dists)))
    lengths <- generate (suchThat (arbitrary :: Gen [Float]) (\x -> (length x == n) && (maximum x <= 300)))
    lengths <- return (lengths `add` replicate (length lengths) ((-1)*(minimum lengths) + 100))
    lengths <- return (lengths `mult` replicate (length lengths) 2)
    heights <- generate (suchThat (arbitrary :: Gen [Float]) (\x -> (length x == 10) && (maximum x <= 200) && (minimum x >= -200)))
    heights <- return (heights `add` replicate (length heights) ((-1)*(minimum heights) + 0))
    blockList <- return (createListOfFloorBlocks n dists lengths heights 0)
    return blockList
        where
            add xs ys	= zipWith (+) xs ys
            mult xs ys	= zipWith (*) xs ys

updateFloorBlocks :: GameState -> [FloorBlock] -> GameState
updateFloorBlocks gstate bs = gstate { level = levelObject { floorBlocks = bs } }
	where
		levelObject = level gstate
