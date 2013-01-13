module Display (display,idle) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects
import Data.IORef
 
import Cube
import Points
 
display angle position = do 
  clear [ColorBuffer,DepthBuffer]
  loadIdentity
  (x,y,z) <- get position
  translate $ Vector3 x y z
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 0.2 (1::GLfloat)
    --scale 0.7 0.7 (0.7::GLfloat)
    mapM_ (\(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
      translate $ Vector3 x y z
    --  renderObject Wireframe $ Sphere' 0.2 10 10
    --  color $ Color3 0 0 (0::GLfloat)
      renderObject Solid $ Sphere' 0.3 100 100
      ) $ points 7
  flush
  --swapBuffers
 
idle angle delta = do
  a <- get angle
  d <- get delta
  angle $=! (a + d) -- The parens are necessary due to a precedence bug in StateVar
  postRedisplay Nothing -- Only required on Mac OS X, which double-buffers internally