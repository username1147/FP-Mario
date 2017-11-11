module Main where

import System.Environment
import Data.Maybe
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

import Controller
import Model
import View
import Types
import Rectangle
import Collision


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


-- main = putStrLn ("Hello, Haskell! Rectangles collide: " ++ (show $ isCollision rect1 rect2))

-- Configuration of the game
screenResolution :: (Int, Int)
screenResolution = (400, 400)

screenOffset :: (Int, Int)
screenOffset = (0, 0)

framesPerSecond :: Int
framesPerSecond = 60

main :: IO ()
main = do
	picture <- loadBMP "src/MARBLES.bmp"
	print "File load"
	print ("Rectangles 1 and 2 collide: " ++ (show $ isCollision testRect1 testRect2))
	print ("Rectangles 1 and 3 collide: " ++ (show $ isCollision testRect1 testRect3))
	print ("Rectangles 2 and 3 collide: " ++ (show $ isCollision testRect2 testRect3))
	playIO (InWindow "Counter" screenResolution screenOffset)
			black -- Background color
			framesPerSecond -- Frames per second
			(initialState screenResolution) -- in Model
			(view picture) -- in View
			input -- in Controller
			step -- in Controller
