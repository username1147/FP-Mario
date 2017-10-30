module Main where

import System.Environment
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy

import Graphics.Gloss.Interface.IO.Game


import Types
import Collision


-- For testing, 2 rectangles...
rect1 = Rectangle {
	topLeft			= Point { x = 2.0, y = 7.0 },
	bottomRight		= Point { x = 5.0, y = 3.0 }
}

rect2 = Rectangle {
	topLeft			= Point { x = 2.0, y = 7.0 },
	bottomRight		= Point { x = 5.0, y = 3.0 }
}


main :: IO ()
main = putStrLn ("Hello, Haskell! Rectangles collide: " ++ (show $ isCollision rect1 rect2))
