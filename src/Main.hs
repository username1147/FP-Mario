module Main where

import System.Environment
import Data.Maybe
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss

import Controller
import Model
import View
import Types
import Collision


-- For testing, 2 rectangles...
testRect1 = Rectangle {
	topLeft			= (2.0, 4.0),
	bottomRight		= (5.0, 2.0)
}

testRect2 = Rectangle {
	topLeft			= (6.0, 7.0),
	bottomRight		= (9.0, 4.0)
}

testRect3 = Rectangle {
	topLeft			= (3.0, 3.0),
	bottomRight		= (6.0, 1.0)
}


-- main = putStrLn ("Hello, Haskell! Rectangles collide: " ++ (show $ isCollision rect1 rect2))


main :: IO ()
main = do
    picture <- loadBMP "src/MARBLES.bmp"
    print "File load"
    print ("Rectangles collide: " ++ (show $ isCollision testRect1 testRect2))
    playIO (InWindow "Counter" (400, 400) (0, 0))
            black -- Background color
            2 -- Frames per second
            initialState -- in Model
            (view picture) -- in View
            input -- in Controller
            step -- in Controller
