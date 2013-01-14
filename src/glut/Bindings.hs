module Bindings (idle,display,reshape,keyboardHandler, specialHandler) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
 
import Display
 
reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)

{-
motionHandler camView camViewNorth mousePos (Position newX newY) = do
  (clicked, oldX, oldY) <- get mousePos
  mousePos $= (True, newX, newY)
  if clicked
  then do
    (x,y,z) <- get camView
    (nx,ny,nz) <- get camViewNorth
    let (rx,ry,rz) = crossProduct (nx,ny,nz) (x,y,z)
    
  else return ()
    
crossProduct (a,b,c) (x,y,z) = 
  (b*z-c*y, c*x - a*z, a*y - b*x)
  -}
  
specialHandler camPos KeyUp _ = do
  (x,y,z) <- get camPos
  camPos $= (x,y,z+0.05)
specialHandler camPos KeyDown _ = do
  (x,y,z) <- get camPos
  camPos $= (x,y,z-0.05)
specialHandler camPos KeyLeft _ = do
  (x,y,z) <- get camPos
  camPos $= (x+0.05,y,z)
specialHandler camPos KeyRight _ = do
  (x,y,z) <- get camPos
  camPos $= (x-0.05,y,z)
specialHandler _ _ _ = return ()
 
keyboardHandler rotSpeed '+' _ = do
  speed <- get rotSpeed
  rotSpeed $= (speed * 2)
keyboardHandler rotSpeed '-' _ = do
  speed <- get rotSpeed
  rotSpeed $= (speed / 2)
keyboardHandler _ '\27' _ = do -- esc
  leaveMainLoop
keyboardHandler _ _ _ = return ()