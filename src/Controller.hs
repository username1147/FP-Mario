module Controller where

import Model

import Graphics.Gloss.Interface.IO.Game

inputKey :: Event -> GameState -> GameState -- Event is defined in the lib.
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState (ShowSquare (n1, n2)) (CameraInfo (x, y) w h) secs) =
    GameState (ShowSquare (n1, n2 + 10)) (CameraInfo (x, y - 10) w h) secs
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gstate = gstate   
inputKey (EventKey (SpecialKey KeyDown) Down _ _) (GameState (ShowSquare (n1, n2)) (CameraInfo (x, y) w h) secs) =
    GameState (ShowSquare (n1, n2 - 10)) (CameraInfo (x, y + 10) w h) secs
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gstate = gstate 
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) (GameState (ShowSquare (n1, n2)) (CameraInfo (x, y) w h) secs) =
    GameState (ShowSquare (n1 - 10, n2)) (CameraInfo (x + 10, y) w h) secs
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate = gstate   
inputKey (EventKey (SpecialKey KeyRight) Down _ _) (GameState (ShowSquare (n1, n2)) (CameraInfo (x, y) w h) secs) =
    GameState (ShowSquare (n1 + 10, n2)) (CameraInfo (x - 10, y) w h) secs
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate = gstate
inputKey (EventKey (SpecialKey KeyEsc) _ _ _) _ = error "Escaped"
inputKey (EventKey (Char c) _ _ _) _ = GameState ShowNothing (cameraInfo initialState) 0
inputKey _ gstate = initialState

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate {elapsedTime = elapsedTime gstate + secs}
