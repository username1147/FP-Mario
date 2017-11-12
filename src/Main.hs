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
	contents <- readFile "src/input.txt"
	inputFileLines <- return (map words $ lines $ contents)
	numbers <- return (map last inputFileLines)
	print (show $ map (read :: (String -> Float)) numbers)

	nr_of_blocks <- return (read (numbers !! 0) :: Int)
	block_max_distance <- return (read (numbers !! 1) :: Float)
	screenResolutionWidth <- return (read (numbers !! 2) :: Int)
	screenResolutionHeight <- return (read (numbers !! 3) :: Int)
	screenOffsetX <- return (read (numbers !! 4) :: Int)
	screenOffsetY <- return (read (numbers !! 5) :: Int)
	fPS <- return (read (numbers !! 6) :: Int)

	outputFile <- return ("src/output.txt" :: FilePath)
	appendFile outputFile "Hello, Mario"
	bls <- generateLevelFloorBlocks nr_of_blocks block_max_distance
	print "File load"
	print (show (bottomLeft $ floorBlockRect $ bls !! 0) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 0))
	print (show (bottomLeft $ floorBlockRect $ bls !! 1) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 1))
	print (show (bottomLeft $ floorBlockRect $ bls !! 2) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 2))
	print (show (bottomLeft $ floorBlockRect $ bls !! 3) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 3))
	print (show (bottomLeft $ floorBlockRect $ bls !! 4) ++ " " ++ show (topRight $ floorBlockRect $ bls !! 4))
	initialStateRandom <- return (updateFloorBlocks (initialState screenResolution) bls)
	initialStateRandomWithEnemies <- return (spawnEnemies initialStateRandom)
	print (show (length $ floorBlocks $ level $ initialStateRandom) ++ " floor blocks")
	print (show (length $ enemies $ initialStateRandomWithEnemies) ++ " enemies")
	print "Controls:"
	print "    Arrow keys:  Move Mario left and right, jump up and move down"
	print "    Esc:         Quit the game"
	print "    P:           Pause the game"
	print "    R:           Reset the game"
	playIO (InWindow "Counter" (screenResolutionWidth, screenResolutionHeight) (screenOffsetX, screenOffsetY))
			black -- Background color
			fPS -- Frames per second
			initialStateRandomWithEnemies -- in Model
			(view picture) -- in View
			input -- in Controller
			step -- in Controller
