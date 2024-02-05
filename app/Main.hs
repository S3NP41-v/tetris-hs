module Main where

import Data.IORef
import Graphics.UI.GLUT
import Control.Monad    ( void )


import Input
import Display
import State



main :: IO ()
main = do
  void $ initialize "Monad Your Gonad" []

  gsRef <- newIORef initialState

  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize  $= Size (fromIntegral xRes) (fromIntegral yRes)

  void $ createWindow "Monad Your Gonad"

  displayCallback         $= display gsRef
  reshapeCallback         $= Just (\_ -> viewport $= (Position 0 0, Size (fromIntegral xRes) (fromIntegral yRes)) >> postRedisplay Nothing)
  keyboardMouseCallback   $= Just (inputHandler gsRef)

  passiveMotionCallback   $= Just (mouseMotion gsRef)
  motionCallback          $= Just (mouseMotion gsRef)

  idleCallback            $= Just (postRedisplay Nothing)

  mainLoop
