module View where

import Model
import Types
import Rectangle

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap


floorBlockColor		= makeColorI 145 87 21 255
pipeColor			= makeColorI 112 211 69 255
itemBlockColor		= makeColorI 145 87 21 127
blockColor			= makeColorI 234 216 14 255
playerColor			= makeColorI 10 58 252 255
enemyColor			= Color red


view :: Picture -> GameState -> IO Picture -- GameState to be defined
view pic = return . (viewPure pic)

-- functions for showing various objects
displayRectangle :: Point -> Rectangle -> Picture
displayRectangle cam rectangle = polygon $ map (levelToScreenCoord . (absToRelCoord cam)) (getCorners rectangle)


viewPure :: Picture -> GameState -> Picture
viewPure pic gstate
	| showNothing 	= pic
	| paused gstate	= Pictures ((map (Color red) (map (displayRectangle cam) allRects)) ++
						[Color yellow (displayRectangle cam playerRect)])
	| otherwise		= Pictures ((map (Color red) (map (displayRectangle cam) allRects)) ++
						[Color green (displayRectangle cam playerRect)])
	where
		showNothing			= infoToShow gstate == ShowNothing
		cam					= cameraPos (camera gstate)
		playerRect			= getRect $ player gstate
		levelMap			= level gstate

		blockRects			= map getRect (blocks levelMap)
		floorBlockRects		= map getRect (floorBlocks levelMap)
		itemBlockRects		= map getRect (itemBlocks levelMap)
		pipeRects			= map getRect (pipes levelMap)

		allRects			= floorBlockRects ++ blockRects ++ pipeRects ++ itemBlockRects



