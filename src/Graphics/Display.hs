module Graphics.Display (
  display,
  idle,
  vectorToVector3,
  ) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects
import Data.IORef
import Criterion.Measurement
import Data.Packed.Vector as V
import Foreign.C.Types
 
import Graphics.Cube
import Graphics.Points
import Simulation
import Scene 
 
display angle position scene = do 
  clear [ColorBuffer,DepthBuffer]
  loadIdentity
  (x,y,z) <- get position
  translate $ Vector3 x y z
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 0.2 (1::GLfloat)
    --scale 0.7 0.7 (0.7::GLfloat)
    
    -- TODO how to manage colors
    color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
    mapM_ ((drawObject =<<) . readIORef) scene
  flush
  --swapBuffers

vectorToVector3 v = Vector3 (CDouble $ v@>0) (CDouble $ v@>1) (CDouble $ v@>2) 

drawObject object = preservingMatrix $ do
  translate $ vectorToVector3 $ x object
  drawContent $ content object

drawContent (Scene.Sphere r _) = renderObject Solid $ Sphere' (CDouble r) 100 100


idle scene angle delta lastSimUpdateRef skippedFramesRef = do
  printScene scene
  lastSimUpdate <- get lastSimUpdateRef
  now <- getTime
  let diff = now - lastSimUpdate
  skippedFrames <- get skippedFramesRef
  if (diff > 0.040 && skippedFrames < 5)  -- 25 Updates / Second
    then do
      skippedFramesRef $= skippedFrames + 1
      updateSimulation scene angle delta
      lastSimUpdateRef $= now
    else do
      skippedFramesRef $= 0
      postRedisplay Nothing -- Only required on Mac OS X, which double-buffers internally