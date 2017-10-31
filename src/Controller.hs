{-# language NamedFieldPuns #-}

module Controller where

import Model

import Graphics.Gloss.Interface.IO.Game

-- supplementary function for inputKey
shiftMario :: Float -> Float -> InfoToShow -> InfoToShow
shiftMario x y (ShowSquare (a, b)) = ShowSquare (a + x, b + y)
shiftMario _ _ ShowNothing = ShowNothing

shiftCameraWithMario :: Float -> Float -> CameraInfo -> CameraInfo
shiftCameraWithMario x y camInfo = camInfo {cornerAbsPos = cornerAbsPos camInfo + (x, y)}

pauseOrResumeGame :: GameState -> GameState
pauseOrResumeGame gstate = gstate {ifPaused = not (ifPaused gstate)}

-- main function for key input
inputKey :: Event -> GameState -> GameState -- Event is defined in the lib.
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate = case ifPaused gstate of
    True -> gstate
    False -> gstate {
                infoToShow = shiftMario 0 10 (infoToShow gstate), 
                cameraInfo = shiftCameraWithMario 0 (-10) (cameraInfo gstate)
             }
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gstate = gstate   
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate = case ifPaused gstate of
     True -> gstate
     False -> gstate {
                    infoToShow = shiftMario 0 (-10) (infoToShow gstate), 
                    cameraInfo = shiftCameraWithMario 0 10 (cameraInfo gstate)
              }
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gstate = gstate 
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate = case ifPaused gstate of
      True -> gstate
      False -> gstate {
                    infoToShow = shiftMario (-10) 0 (infoToShow gstate), 
                    cameraInfo = shiftCameraWithMario 10 0 (cameraInfo gstate)
               }
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) gstate = gstate   
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = case ifPaused gstate of
     True -> gstate
     False -> gstate {
                    infoToShow = shiftMario 10 0 (infoToShow gstate), 
                    cameraInfo = shiftCameraWithMario (-10) 0 (cameraInfo gstate)
              }
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gstate = gstate
inputKey (EventKey (SpecialKey KeyEsc) _ _ _) _ = error "Escaped"
inputKey (EventKey (Char 'p') Down _ _) gstate = pauseOrResumeGame gstate
inputKey (EventKey (Char 'p') Up _ _) gstate = gstate
inputKey (EventKey (Char 'r') Up _ _) gstate = initialState -- r = revert back to init.
inputKey (EventKey (Char 'c') _ _ _) _ = GameState ShowNothing (cameraInfo initialState) False 0
inputKey _ gstate = initialState

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate {elapsedTime = elapsedTime gstate + secs}
