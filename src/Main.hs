module Main where

import Controller
import Model
import View

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss


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
