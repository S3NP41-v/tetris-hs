module State 
    ( yRes, xRes, frameRate, blockScale, blockShadow
    , initialState
    , GameState(..), Board(..), Block(..)
    ) where


import Graphics.UI.GLUT
import Data.IORef


-- this is a file to hold all default values
-- because i hate hard coded values that are repeated
--  all over the code, instead this can serve as a
--  config file for those that want to tinker with
--  the code.


yRes, xRes :: Int
yRes = 420
xRes = 360

frameRate :: Int
frameRate = 60

blockScale, blockShadow :: Float
blockScale  = 20
blockShadow = 2


initialState :: GameState
initialState = GameState 
    { frame     = 0
    , board     = Board [[]]
    , inputs    = []
    , score     = 0
    , stored    = Nothing
    , next      = Block
    , speed     = 0

    , blockPos  = Position 185 185
    }

data GameState = GameState
    { frame  :: Integer
    , board  :: Board
    , inputs :: [(Key, KeyState)]
    , score  :: Int
    , stored :: Maybe Block
    , next   :: Block
    , speed  :: Integer

    , blockPos :: Position
    }


newtype Board = Board [[Block]]

data Block = Block



