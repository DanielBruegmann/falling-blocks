module Simulation where

import Data.IORef

import Scene
import Object

updateSimulation scene angleRef deltaRef = do
  putStrLn ".."
  angle <- readIORef angleRef
  delta <- readIORef deltaRef
  angleRef `writeIORef` (angle + delta)
  moveObjects scene 1

moveObjects :: [IORef Object] -> Double -> IO ()
moveObjects scene timestep = mapM_ (\s -> moveObject s timestep) scene