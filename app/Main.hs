module Main where

import Data.IORef
import Graphics.UI.GLUT
import Control.Monad    ( void )


import Input
import Display
import State


{-  Info About The Tetris Game
# Scoring:
#  levels:
#    levels control how fast a tetromino falls, start at level 0 which is a cell fallen every ??? frames
#    level 29: maximum level, tetromino's fall a cell for every frame
#    levels advance for every 10 lines cleared (not to confuse with 10 line-clears)
#  score:
#    40 * (l + 1) for the single line clear
#    100 * (l + 1) for the double line clear
#    300 * (l + 1) for the triple line clear
#    1200 * (l + 1) for the quadruple line clear (a tetris)
#    1 for every cell fallen by holding the down key
#    where l = current level
# Input:
#  when a movement key is pressed, the piece will instantly move one grid cell, stop for 16 frames due to delayed auto-shift, 
#  before moving again once every 6 frames (10 times a second, as the game runs at 60 fps)
-}

{-  Plans For The Future
# -Use actual original rotation points for tetrominos, the I and O types rotate around a point i picked,
#   in the future i will rotate them around their actual canon points
#
# -Making the UI pretty
#
# -Save highscore
#
# -Settings to save/load +defaults (maybe just a .json file?)
#
# -Music and other SFX
#
# -A menu (corelated with settings)
#
# -Frame absed input, right now its one press one action,
#   so probably move the handling to gameLogicLoop
#
# -Overall better code, hopefully with the help of FP Discord community this project will be able to serve as an example of learning
#   and good code for other beginners.
#   -Overhaul to the GameState record type and its accompanying functions, i feel like there is a ton of lost potential there
-}


main :: IO ()
main = do
  void $ initialize "Monad Your Gonad" []

  gsRef <- newIORef initialState
  
  (pulls', current' : next' : _) <- manyPull (pulls initialState) 2
  
  readIORef gsRef >>= \gs -> gsRef $= gs{next = Just next', currentTetromino = Just (moveTetromino dropPoint current'), pulls = pulls'}

  initialDisplayMode $= [RGBAMode, DoubleBuffered]
  initialWindowSize  $= Size (fromIntegral xRes) (fromIntegral yRes)

  void $ createWindow "Monad Your Gonad"

  displayCallback         $= display gsRef
  reshapeCallback         $= Just (\_ -> viewport $= (Position 0 0, Size (fromIntegral xRes) (fromIntegral yRes)) >> postRedisplay Nothing)
  keyboardMouseCallback   $= Just (inputHandler gsRef)

  -- passiveMotionCallback   $= Just (mouseMotion gsRef)
  -- motionCallback          $= Just (mouseMotion gsRef)

  idleCallback            $= Just (gameLogicLoop gsRef)

  mainLoop
