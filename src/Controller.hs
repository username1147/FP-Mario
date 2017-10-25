module Controller where

import Model

import Graphics.Gloss.Interface.IO.Game

inputKey :: Event -> GameState -> GameState -- Event is defined in the lib.
inputKey (EventKey (SpecialKey KeyUp) _ _ _) (GameState (ShowSquare (n1, n2)) cam secs) =
    GameState (ShowSquare (n1, n2 + 1)) cam secs
inputKey (EventKey (SpecialKey KeyDown) _ _ _) (GameState (ShowSquare (n1, n2)) cam secs) =
    GameState (ShowSquare (n1, n2 - 1)) cam secs
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) (GameState (ShowSquare (n1, n2)) cam secs) =
    GameState (ShowSquare (n1 - 1, n2)) cam secs
inputKey (EventKey (SpecialKey KeyRight) _ _ _) (GameState (ShowSquare (n1, n2)) cam secs) =
    GameState (ShowSquare (n1 + 1, n2)) cam secs
inputKey (EventKey (SpecialKey KeyEsc) _ _ _) _ = error "Escaped"
inputKey (EventKey (Char c) _ _ _) _ = GameState ShowNothing (cameraInfo initialState) 0
inputKey _ gstate = initialState

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate {elapsedTime = elapsedTime gstate + secs}
