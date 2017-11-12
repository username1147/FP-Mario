module Main where

import System.Environment
import Data.Maybe
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Test.QuickCheck

import Controller
import Model
import View
import Types
import Rectangle
import Collision
import Actions
import RandomGen

-- For testing, 3 rectangles...
testRect1 :: Rectangle
testRect1 = Rectangle {
	bottomLeft	= (2.0, 2.0),
	topRight	= (5.0, 4.0)
}

testRect2 :: Rectangle
testRect2 = Rectangle {
	bottomLeft	= (6.0, 4.0),
	topRight	= (9.0, 7.0)
}

testRect3 :: Rectangle
testRect3 = Rectangle {
	bottomLeft	= (3.0, 1.0),
	topRight	= (6.0, 3.0)
}


-- Configuration of the game
screenResolution :: (Int, Int)
screenResolution = (800, 600)

screenOffset :: (Int, Int)
screenOffset = (0, 0)

framesPerSecond :: Int
framesPerSecond = 60



{-
instance Arbitrary Enemy where
	arbitrary = do	p1 <- arbitrary
					p2 <- arbitrary
					elements [
						Enemy {enemyRect = Rectangle p1 p2, enemyActions = defaultAction},
						Enemy {enemyRect = Rectangle p2 p1, enemyActions = defaultAction}]
-}

main :: IO ()
main = do
	picture	<- loadBMP "src/MARBLES.bmp"
	frames	<- generate (arbitrary :: Gen [Point])
	number	<- generate (choose (1, 10) :: Gen Int)
	bls <- generateLevelFloorBlocks 5 50
	print ("Random integers:" ++ (show frames) ++ ", sum: " ++ show (sum frames))
	print (show (bottomLeft $ floorBlockRect $ bls !! 0) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 0))
	print (show (bottomLeft $ floorBlockRect $ bls !! 1) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 1))
	print (show (bottomLeft $ floorBlockRect $ bls !! 2) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 2))
	print (show (bottomLeft $ floorBlockRect $ bls !! 3) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 3))
	print (show (bottomLeft $ floorBlockRect $ bls !! 4) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 4))
	initialStateRandom <- return (updateFloorBlocks (initialState screenResolution) bls)
	print (show (length $ floorBlocks $ level $ initialStateRandom))
	playIO (InWindow "Counter" screenResolution screenOffset)
			black -- Background color
			framesPerSecond -- Frames per second
			initialStateRandom -- in Model
			(view picture) -- in View
			input -- in Controller
			step -- in Controller
