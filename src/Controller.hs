{-# language NamedFieldPuns #-}

module Controller where

import Graphics.Gloss.Interface.IO.Game
import Types
import Model
import Rectangle

-- supplementary function for inputKey
shiftCameraWithMario :: Float -> Float -> Camera -> Camera
shiftCameraWithMario x y cam = cam { cameraPos = cameraPos cam + (x, y)}

togglePause :: GameState -> GameState
togglePause gstate = gstate {paused = not (paused gstate)}

-- main function for key input
inputKey :: Event -> GameState -> GameState -- Event is defined in the lib.
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerRect = shiftRectangle (getRect playerObject) (0, 10) }
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (0, -10) }
			}
	where
		playerObject = player gstate
		cameraObject = camera gstate
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gstate = gstate
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerRect = shiftRectangle (getRect playerObject) (0, -10) }
				-- camera = cameraObject { cameraPos = cameraPos cameraObject + (0, 10) }
			}
	where
		playerObject = player gstate
		cameraObject = camera gstate
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gstate = gstate
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerRect = shiftRectangle (getRect playerObject) (-10, 0) },
				camera = cameraObject { cameraPos = cameraPos cameraObject + (10, 0) }
			}
	where
		playerObject = player gstate
		cameraObject = camera gstate

inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate = gstate
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = case paused gstate of
	True -> gstate
	False -> gstate {
				player = playerObject { playerRect = shiftRectangle (getRect playerObject) (10, 0) },
				camera = cameraObject { cameraPos = cameraPos cameraObject + (-10, 0) }
			}
	where
		playerObject = player gstate
		cameraObject = camera gstate
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate = gstate
inputKey (EventKey (SpecialKey KeyEsc) _ _ _) _ = error "Escaped"
inputKey (EventKey (Char 'p') Down _ _) gstate = togglePause gstate
inputKey (EventKey (Char 'p') Up _ _) gstate = gstate
inputKey (EventKey (Char 'r') Up _ _) gstate = initialState -- r = revert back to init.
inputKey (EventKey (Char 'c') _ _ _) gstate = gstate {
	infoToShow = ShowNothing,
	camera = camera initialState,
	paused = False
}

inputKey _ gstate = initialState

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate {elapsedTime = elapsedTime gstate + secs}
