module Simulation where

import Graphics.UI.GLUT

updateSimulation angleRef deltaRef = do
  angle <- get angleRef
  delta <- get deltaRef
  angleRef $=! (angle + delta) -- The parens are necessary due to a precedence bug in StateVar
