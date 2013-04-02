{-# LANGUAGE TupleSections #-}
module Object where

import Numeric.LinearAlgebra
import Control.Applicative((<$>))
import Control.Monad
import Data.StateRef
import Data.IORef
import Data.List(tails,minimumBy)
import Data.Maybe(listToMaybe,maybeToList)
import Data.Function(on)

import Scene

vec :: Double -> Vector Double
vec x = fromList [x]

moveObject :: IORef Object -> Double -> IO ()
moveObject objectRef timestep = modifyIORef objectRef (\object -> object{x=(vec timestep) * (v object) + (x object)})

whenCollision :: Object -> Object -> Maybe Double
-- TODO v1 == v2
whenCollision (Object x1 v1 (Sphere r1 _)) (Object x2 v2 (Sphere r2 _)) = if disc >= 0 then
      listToMaybe $ filter (>=0) [t_minus, t_plus]
    else 
      Nothing
  where 
    t_plus = - p/2 + sqrt disc
    t_minus = - p/2 - sqrt disc
    p = 2 * (dx <.> dv)/(dv <.> dv)
    disc = (p/2)^2 - q
    q = ((dx <.> dx) - (r1 + r2)^2) / (dv <.> dv)
    dx = x1 - x2
    dv = v1 - v2 

whenCollision so@(Object _ _ (Sphere _ _)) po@(Object _ _ (Plane _)) = whenCollisionSpherePlane so po
whenCollision po@(Object _ _ (Plane _)) so@(Object _ _ (Sphere _ _)) = whenCollisionSpherePlane so po
whenCollision (Object _ _ (Plane _)) (Object _ _ (Plane _)) = Nothing -- Planes do not collide

whenCollisionSpherePlane (Object x1 v1 (Sphere r _)) (Object x2 v2 (Plane n)) = error "whenCollisionSpherePlane unimplemented"
-- get rid of non-exhaustive pattern match and type-check order of arguments

performCollision r1 r2 = do
  o1 <- readIORef r1
  o2 <- readIORef r2
  case (o1,o2) of
    ((Object _ _ (Sphere _ _)),(Object _ _ (Sphere _ _))) ->
      let
        m1 = m (content o1)
        m2 = m (content o2)
        v_CenterOfMass =  vec (1/(m1 + m2)) * (vec m1 * v o1 + vec m2 * v o2) 
      in do
        writeIORef r1 o1{ v = 2 * v_CenterOfMass - v o1 }
        writeIORef r2 o2{ v = 2 * v_CenterOfMass - v o2 }
    (_,_) -> error "performCollision unimplemented"

addCurrentValuesToObjectRefs :: [IORef Object] -> IO [(IORef Object, Object)] 
addCurrentValuesToObjectRefs = sequence . map (\r -> (r,) <$> readIORef r) 

nextCollision :: [(IORef Object, Object)] -> Maybe (Double, (IORef Object, IORef Object)) -- TODO
nextCollision objects = case collisions of 
  [] -> Nothing
  _ -> Just $ (\(t,((r1,o1),(r2,o2))) -> (t,(r1,r2))) <$> minimumBy (compare `on` fst) $ collisions
  where
    collisions = concat $ map firstWithAllInTail $ tails objects
    firstWithAllInTail (o:os) = concat $ flip map os (\o2 -> 
      maybeToList $ (,(o,o2)) <$> whenCollision (snd o) (snd o2)) 
    firstWithAllInTail [] = []