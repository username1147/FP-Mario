module View where

import Model
import Types
import Util

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap


view :: Picture -> GameState -> IO Picture -- GameState to be defined
view pic = return . (viewPure pic)

-- functions for showing various objects
displayRectangle :: Point -> Rectangle -> Picture
displayRectangle cam rectangle = polygon $ map (levelToScreenCoord . (absToRelCoord cam)) (getCorners rectangle)


viewPure :: Picture -> GameState -> Picture
viewPure pic gstate = case infoToShow gstate of -- different game states to be def.
    ShowNothing	-> pic
    ShowGame 	-> Pictures ((map (Color red) (map (displayRectangle cam) allRects) ++
                            [Color green (displayRectangle cam playerRect)])
	where
		cam					= cornerAbsPos (camera gstate)
		levelMap			= level gstate
		blockRects			= map getRect (blocks levelMap)
		floorBlockRects		= map getRect (floorBlocks levelMap)
		itemBlockRects		= map getRect (itemBlocks levelMap)
		pipeRects			= map getRect (pipes levelMap)
		allRects 			= floorRects ++ pipeRects ++ blockRects ++ itemBlockRects
		playerRect			= getRect $ player gstate
