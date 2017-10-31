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
rect1 = Rectangle {
	topLeft			= Point { x = 2.0, y = 4.0 },
	bottomRight		= Point { x = 5.0, y = 2.0 }
}

rect2 = Rectangle {
	topLeft			= Point { x = 6.0, y = 7.0 },
	bottomRight		= Point { x = 9.0, y = 4.0 }
}

rect3 = Rectangle {
	topLeft			= Point { x = 3.0, y = 3.0 },
	bottomRight		= Point { x = 6.0, y = 1.0 }
}


-- main = putStrLn ("Hello, Haskell! Rectangles collide: " ++ (show $ isCollision rect1 rect2))


main :: IO ()
main = do
    picture <- loadBMP "src/MARBLES.bmp"
    print "File load"
    playIO (InWindow "Counter" (400, 400) (0, 0))
            black -- Background color
            2 -- Frames per second
            initialState -- in Model
            (view picture) -- in View
            input -- in Controller
            step -- in Controller
