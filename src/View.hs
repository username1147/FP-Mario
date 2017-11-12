module View where

import Model
import Types
import Rectangle
import Collision

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap


floorBlockColor		= makeColorI 145 87 21 255
pipeColor			= makeColorI 112 211 69 255
itemBlockColor		= makeColorI 145 87 21 127
blockColor			= makeColorI 234 216 14 255
-- playerColor			= makeColorI 10 58 252 255
enemyColor			= Color red



absToCamCoord :: Point -> Point -> Point -- abs. pos -> screen abs. pos.
absToCamCoord camPos point = point - camPos

-- TODO: Add adjustable screen resolution
-- because left-bottom of level has coord. (0, 0), for the screen it is (-width/2, -height/2)
-- camCoordToScreenCoord :: Point -> Resolution -> Point
camCoordToScreenCoord :: Point -> Point -> Point
camCoordToScreenCoord (halfResWidth, halfResHeight) (x, y) = (x - halfResWidth, y - halfResWidth)



view :: Picture -> GameState -> IO Picture
view pic = return . (viewPure pic)

-- Function for showing a rectangle as a Picture
-- createRectanglePicture :: Point -> Resolution -> Rectangle -> Picture
createRectanglePicture :: Point -> Point -> Rectangle -> Picture
createRectanglePicture cam halfRes rectangle = polygon $ map ((camCoordToScreenCoord halfRes) . (absToCamCoord cam)) (getCorners rectangle)


viewPure :: Picture -> GameState -> Picture
viewPure pic gstate
	| showNothing 	= pic
	| paused gstate	= Pictures ((map (Color red) (map (createRectanglePicture camPos halfRes) allRects)) ++
						[Color yellow (createRectanglePicture camPos halfRes playerRect), infoPicture])
	| otherwise		= Pictures ((map (Color red) (map (createRectanglePicture camPos halfRes) allRects)) ++
						[Color playerColor (createRectanglePicture camPos halfRes playerRect), infoPicture])
	where
		halfRes				= convertToFloatTuple $ resolutionHalf gstate
		showNothing			= infoToShow gstate == ShowNothing
		camPos				= cameraPos (camera gstate)
		playerRect			= getRect $ player gstate
		levelMap			= level gstate

		blockRects			= map getRect (blocks levelMap)
		floorBlockRects		= map getRect (floorBlocks levelMap)
		itemBlockRects		= map getRect (itemBlocks levelMap)
		pipeRects			= map getRect (pipes levelMap)

		allRects			= floorBlockRects ++ blockRects ++ pipeRects ++ itemBlockRects

		playerCollides		= elem True $ map (isCollision playerRect) allRects
		playerColor			= if playerCollides then yellow else green

		textPicture			= color green (text ("Last frametime: " ++ show (lastFrameTime gstate)))
		scaledTextPicture	= scale 0.1 0.1 textPicture
		infoPicture			= translate (-190.0) (170.0) scaledTextPicture

