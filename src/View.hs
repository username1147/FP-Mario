module View where

import Model

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap


view :: Picture -> GameState -> IO Picture -- GameState to be defined
view pic = return . (viewPure pic)

-- functions for showing various objects
showRectObject :: Point -> RectObject -> Picture
showRectObject cam (RectObject (p1, p2, p3, p4)) = polygon $ map (levelToScreenCoord . (absToRelCoord cam)) [p1, p2, p3, p4]

smallGreenSquare :: Point -> Picture
smallGreenSquare (n1, n2) = polygon [(n1, n2), (n1, n2 + 10), (n1 + 10, n2 + 10), (n1 + 10, n2)]
-- coordinates are x and y w.r.t. to centre of the window

viewPure :: Picture -> GameState -> Picture
viewPure pic gstate = case infoToShow gstate of -- different game states to be def.
    ShowNothing -> pic 
    ShowSquare p -> Pictures [Color red (showRectObject cam rect1), 
                            Color red (showRectObject cam rect2),
                            Color green (smallGreenSquare ((levelToScreenCoord . (absToRelCoord cam)) p))]
                        where cam = cornerAbsPos (cameraInfo gstate)
