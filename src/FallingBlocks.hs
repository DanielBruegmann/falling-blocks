import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Criterion.Measurement
 
import Graphics.Bindings
import Object
import Scene
 
main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer]
  --initialDisplayMode $= [DoubleBuffered]
  createWindow "Hello World"
  --windowSize $= Size 500 500
  reshapeCallback $= Just reshape
  
  scene <- createScene

  lighting $= Enabled
  position (Light 0) $= Vertex4 3 0 0  1
  light (Light 0) $= Enabled
  depthFunc $= Just Less -- specifies comparison function for DepthBuffer
  angle <- newIORef (0.0::GLfloat)
  rotSpeed <- newIORef (0.01::GLfloat)
  camPos <- newIORef (0.0::GLfloat, 0.0, -2.0)
  a <- getTime
  lastSimUpdate <- newIORef (a)
  skippedFrames <- newIORef (0::Int)
  --camView <- newIORef (0.0::GLfloat, 0.0, 1.0)
  --camViewNorth <- newIORef (0.0::GLfloat, 1.0, 0.0) -- wo ist oben?
  --mousePos <- newIORef (False, 0::GLint,0) -- ist Maus geklickt? letzte Koordinaten
  keyboardCallback $= Just (keyboardHandler rotSpeed)
  specialCallback $= Just (specialHandler camPos)
  --motionCallback $= Just (motionHandler camView camViewNorth mousePos)
  idleCallback $= Just (idle scene angle rotSpeed lastSimUpdate skippedFrames)
  displayCallback $= (display angle camPos scene)

  matrixMode $= Projection 
  loadIdentity
  let near   = 1
      far    = 400
      right  = 1
      top    = 1
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0

  mainLoop