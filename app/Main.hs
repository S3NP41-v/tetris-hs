module Main where

import Graphics.UI.GLUT
 

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Monad Your Gonad"
  displayCallback $= display
  reshapeCallback $= Just reshape

  mainLoop


display :: DisplayCallback
display = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
      vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
  clear [ColorBuffer]
  renderPrimitive Triangles $ do
    color3f  1 0 0
    vertex3f 0 0.5 0
    vertex3f 0.5 0 0
    vertex3f (-0.5) 0 0

  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing