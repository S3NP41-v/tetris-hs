module Input where

import Graphics.UI.GLUT
import Data.IORef
import System.Exit    ( exitSuccess )
import Control.Monad  ( when )

import State


inputHandler :: IORef GameState -> KeyboardMouseCallback
inputHandler gsRef key kState _ _ = case (key, kState) of
  (SpecialKey KeyLeft,  Down) -> moveOnKey gsRef (-1, 0)    -- move piece left
  (SpecialKey KeyRight, Down) -> moveOnKey gsRef (1, 0)     -- move piece right
  (SpecialKey KeyDown,  Down) -> moveOnKey gsRef (0, 1) >> readIORef gsRef >>= \gs -> gsRef $= gs{score = score gs + 1}
  (SpecialKey k , Up)         -> readIORef gsRef >>= \gs -> gsRef $= gs{inputs = take 10 $ (frame gs, (SpecialKey k, Up)) : inputs gs}
  (Char 'q', Down)            -> exitSuccess                -- TODO: maybe save highscore
  (Char 'z', Down)            -> storageOnKey gsRef         -- store/unstore piece
  (Char 'x', Down)            -> rotateOnKey gsRef ToLeft   -- rotate left
  (Char 'c', Down)            -> rotateOnKey gsRef ToRight  -- rotate right

  (Char ' ', Down)            -> undefined
  _                           -> return ()
inputHandler _ _ _ _ _        = return ()


rotateOnKey :: IORef GameState -> Rotation -> IO ()
rotateOnKey gsRef r = do
    gs <- readIORef gsRef

    case currentTetromino gs of
      Just t  -> when (canRotate r t (unmovableTetrominos gs)) $ gsRef $= gs{currentTetromino = Just (rotateTetromino r t)}
      Nothing -> pure ()


moveOnKey :: IORef GameState -> (Int, Int) -> IO ()
moveOnKey gsRef xy = do
  gs <- readIORef gsRef

  case currentTetromino gs of
    Just t  -> when (canMove xy t (unmovableTetrominos gs)) $ gsRef $= gs{currentTetromino = Just (moveTetromino xy t)}
    Nothing -> pure ()


storageOnKey :: IORef GameState -> IO ()
storageOnKey gsRef = do
  gs <- readIORef gsRef
  
  case stored gs of
    Nothing -> do  -- if the storage is empty
      (pulls, nT) <- pullTetromino (pulls gs)
      case currentTetromino gs of
        Just (Tetromino _ _ t) -> gsRef $= gs{currentTetromino = next gs, stored = Just (realTetromino t), next = Just nT, pulls = pulls} -- and there is a current tetromino
        Nothing                -> gsRef $= gs{currentTetromino = next gs, stored = Nothing,                next = Just nT, pulls = pulls} -- and there is no current tetromino
    Just (Tetromino _ _ t) -> gsRef $= gs{currentTetromino = Just (moveTetromino dropPoint (realTetromino t)), stored = currentTetromino gs} -- if the storage has a tetromino
