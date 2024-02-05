module Input where

import Graphics.UI.GLUT
import Data.IORef
import System.Exit ( exitSuccess )

import State ( GameState (..) )




inputHandler :: IORef GameState -> KeyboardMouseCallback
inputHandler gsRef key Down _ _ = case key of
    (SpecialKey KeyLeft ) -> undefined
    (SpecialKey KeyRight) -> undefined
    (SpecialKey KeyUp   ) -> undefined  -- rotate
    (SpecialKey KeyDown ) -> undefined  -- move down as long as pressed (and within move frame)
    (Char 'q')            -> exitSuccess -- maybe save highscore
    (Char 'z')            -> undefined  -- store/unstore piece
    (Char 'x')            -> undefined  -- alt rotate
    (Char ' ')            -> undefined  -- slam piece
    _                     -> return ()
inputHandler _ _ _ _ _     = return ()
