import Sphere
import Data.IORef
import Data.StateRef
import Numeric.LinearAlgebra
import Data.Packed.Vector as V


-- the output should be 1.9999..., the initial data built by calculating where the second ball should start for the collision to occur
-- after 2 time units (t = 2 in the where-clause)
main = do 
  let s1 = (Object x1 v1 (Sphere 2))
  let s2 = (Object x2 v2 (Sphere 1))
  s1ref <- newIORef s1
  s2ref <- newIORef s2
  maybe (putStrLn "No collision.") (\(time,_) -> putStrLn $ "Collision at " ++ show time ++ ".") $ nextCollision [(s1ref,s1), (s2ref,s2)]
--   print s2
--   print $ whenCollision s1 s2
  where
    -- collision at t=0.5
--     x1 = V.fromList [0,0] 
--     x2 = zipVectorWith (+) x1 (zipVectorWith (+) (mapVector (2*) v1) (mapVector ((-2)*) v2)) 
--     v1 = V.fromList [1,0]
--     v2 = V.fromList [-1,0]
    t = 2
    r1 = 2
    r2 = 1
    x1 = V.fromList [0,0] 
    x2 = zipVectorWith (+) x1 (zipVectorWith (+) (V.fromList [r1+r2,0]) (zipVectorWith (+) (mapVector (t*) v1) (mapVector ((-t)*) v2))) 
    v1 = V.fromList [1,-1]
    v2 = V.fromList [-1,-4]