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
import Util


-- For testing, 2 rectangles...
testRect1 = Rectangle {
    bottomLeft      = (2.0, 2.0),
    topRight        = (5.0, 4.0)
}

testRect2 = Rectangle {
    bottomLeft      = (6.0, 4.0),
    topRight        = (9.0, 7.0)
}

testRect3 = Rectangle {
    bottomLeft      = (3.0, 1.0),
    topRight        = (6.0, 3.0)
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
