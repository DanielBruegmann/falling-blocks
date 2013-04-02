{-# LANGUAGE TupleSections #-}
module Simulation where

import Control.Applicative

import Data.IORef

import Scene
import Object

updateSimulation scene angleRef deltaRef = do
  putStrLn ".."
  angle <- readIORef angleRef
  delta <- readIORef deltaRef
  angleRef `writeIORef` (angle + delta)
  objectList <- addCurrentValuesToObjectRefs scene
  let 
    -- If two collision occur at the same time, it will only handle one of them.
    handleCollisions step | step <= 0 = return ()
    handleCollisions step = case nextCollision objectList of -- TODO use maybe monad to reduce moveObjects ... duplication
      Just (t, (r1,r2)) -> do { putStrLn ("t: " ++ show t); if t > step then moveObjects scene step 
        else if t > 0 then do
          moveObjects scene t 
          performCollision r1 r2
          handleCollisions (step - t)
        else moveObjects scene step}
      Nothing -> moveObjects scene step
    in handleCollisions 1

moveObjects :: [IORef Object] -> Double -> IO ()
moveObjects scene timestep = mapM_ (\s -> moveObject s timestep) scene