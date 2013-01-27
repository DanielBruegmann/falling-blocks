module Scene where

import Data.IORef
import Data.Packed.Vector as V
import Control.Monad

type R3 = Vector Double -- length in type?

--objectRefs :: [IOref Object]

data Object = Object { 
  x :: R3,
  v :: R3,
  content :: Type
} deriving Show

data Type = Sphere Double |
            Plane R3 deriving Show

printScene :: [IORef Object] -> IO ()
printScene scene = mapM_ (\ref -> print =<< readIORef ref) scene
            
createObject :: (Double, Double, Double) -> (Double, Double, Double) -> Type -> Object
createObject (x1, x2, x3) (v1, v2, v3) myContent = Object{x=myX, v=myV, content=myContent} 
  where
    myX = V.fromList [x1,x2,x3]
    myV = V.fromList [v1,v2,v3]
            
createScene :: IO [IORef Object]
createScene = sequence [newIORef (object)]
  where
    object = createObject (0,0,0) (1,2,3) (Sphere 1) :: Object

{-
convertToObject :: String -> IO (IORef Object)
convertToObject line = do
  let listOfWords = words line
  newIORef (Object {x=(read "FromList [0,0,0]" :: V.Vector Double), v=(read "FromList [0,0,0]" :: V.Vector Double), content=Sphere 0})

readFromFile :: String -> IO [IORef Object]
readFromFile filename = do
  content <- readFile filename 
  let listOfLines = lines content
  mapM convertToObject listOfLines
  -}