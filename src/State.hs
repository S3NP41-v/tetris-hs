{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module State
    ( yRes, xRes, frameRate, blockScale, blockShadow, dropPoint, initialState
    , GameState(..), Rotation(..), Mino(..), Tetromino(..), TetrominoType(..)
    , canRotate, canMove
    , realTetromino, pullTetromino, manyPull, rotateTetromino, moveTetromino, moveMTetromino, mapBody
    , gameLogicLoop
    ) where


import Graphics.UI.GLUT hiding ( Level )

import Data.IORef
import Control.Monad    ( when )
import Data.List        ( sortBy )
import System.Random    ( getStdRandom, randomR)

import Utils            ( updatePair, (~+), (~:), inBounds, (~++), conMap )
import System.Exit (exitSuccess)


{--
# This file contains a lot of the default values, making the game possible to configure easily by anyone wanting to tinker with the code.
# Also, a lot of underlaying strict-game-logic happens here, so all in all this is the most important file
--}

-- DEFAULT VALUES
yRes, xRes :: Int
yRes = 420
xRes = 360

frameRate :: Int
frameRate = 60

blockScale, blockShadow :: Float
blockScale  = 20
blockShadow = 2

dropPoint :: (Int, Int)
dropPoint = (5, 2)



initialState :: GameState
initialState = GameState
  { frame     = 0
  , score     = 0
  , level     = 0
  , next      = Nothing
  , stored    = Nothing

  , currentTetromino    = Nothing
  , unmovableTetrominos = []

  , inputs    = []
  , pulls     = zip tetrominoTypes (replicate (length tetrominoTypes) 0)
  }
-- /DEFAULT VALUES


data GameState = GameState
  { frame  :: Frame
  , score  :: Score
  , level  :: Level

  , next   :: Maybe Tetromino
  , stored :: Maybe Tetromino

  , currentTetromino    :: Maybe Tetromino
  , unmovableTetrominos :: [Mino]

  , inputs :: [(Frame, (Key, KeyState))]
  , pulls  :: Pulls
  } deriving ( Show )



gameLogicLoop :: IORef GameState -> IO ()
gameLogicLoop gsRef = do
  gs <- readIORef gsRef

  -- TODO: game over
  gameOver (unmovableTetrominos gs)


  -- advance piece
  when (frame gs `rem` toInteger (30 - level gs) == 0) $ do
    case currentTetromino gs of
      Nothing -> return ()
      Just t  -> if canMove (0, 1) t (unmovableTetrominos gs)
        then gsRef $= gs{currentTetromino = Just (moveTetromino (0, 1) t)}
        else do
          (pulls, nx) <- pullTetromino (pulls gs)

          gsRef $= gs
            { currentTetromino    = moveMTetromino dropPoint (next gs)
            , unmovableTetrominos = mTetBody (currentTetromino gs) ~++ unmovableTetrominos gs
            , next                = Just nx
            , pulls               = pulls
            }

  -- TODO: line clears
  -- let (score', nTs) = lineClears (unmovableTetrominos gs)


  postRedisplay Nothing  -- just so the state is actually rendered


gameOver :: [Mino] -> IO ()
gameOver ts = when (any (\(Mino (_, y) _) -> y <= snd dropPoint) ts) $ do
  putStrLn "Game Over"
  exitSuccess


-- lineClears :: [(BoardPos2D, Mino)] -> (Score, [(BoardPos2D, Mino)])
-- lineClears = lineClears' 19

-- todo, get score from this
lineClears' :: Int -> [Mino] -> [Mino]
lineClears' 0 _  = []
lineClears' y ms
  | length row == 10 = lineClears' (y - 1) (filter (`notElem` row) (conMap (moveMino (0, 1)) (\(Mino (_, y') _) -> y > y') ms))
  | otherwise        = lineClears' (y - 1) ms
  where
    row = filter (\(Mino (_, y') _) -> y == y') ms




rotateTetromino :: Rotation -> Tetromino -> Tetromino
rotateTetromino ToLeft  t@(Tetromino _ (x', y') _) = mapBody (\(Mino (x, y) c) -> Mino ((y - y') + x', -(x - x') + y') c) t
rotateTetromino ToRight t@(Tetromino _ (x', y') _) = mapBody (\(Mino (x, y) c) -> Mino (-(y - y') + x', (x - x') + y') c) t

moveTetromino :: (Int, Int) -> Tetromino -> Tetromino
moveTetromino xy (Tetromino ms r t) = Tetromino (map (moveMino xy) ms) (r ~+ xy) t

moveMTetromino :: (Int, Int) -> Maybe Tetromino -> Maybe Tetromino
moveMTetromino xy (Just (Tetromino ms r t)) = Just $ Tetromino (map (moveMino xy) ms) (r ~+ xy) t
moveMTetromino _ Nothing                    = Nothing

moveMino :: (Int, Int) -> Mino -> Mino
moveMino xy' (Mino xy c) = Mino (xy ~+ xy') c

-- can the tetromino rotate in the rotation specified
canRotate :: Rotation -> Tetromino -> [Mino] -> Bool
canRotate r t = spaceCheck (map (\(Mino xy _) -> xy) (tetBody $ rotateTetromino r t))

-- can the tetromino move in the direction specified by a BoardPos2D vector
canMove :: (Int, Int) -> Tetromino -> [Mino] -> Bool
canMove d t = spaceCheck (map (\(Mino xy _) -> (d ~+ xy)) (tetBody t))


spaceCheck :: [(Int, Int)] -> [Mino] -> Bool
spaceCheck pMove bs = all (\(x, y) -> inBounds x (0, 9) && inBounds y (0, 20)) pMove
              && all (\(Mino xy _) -> xy `notElem` pMove) bs

-- variant that lets you pull multiple tetrominos at a time
manyPull :: Pulls -> Int -> IO (Pulls, [Tetromino])
manyPull pulls i = manyPull' pulls i []

manyPull' :: Pulls -> Int -> [Tetromino] -> IO (Pulls, [Tetromino])
manyPull' pulls 0 ts = return (pulls, ts)
manyPull' pulls i ts = pullTetromino pulls >>= \(pulls', t) -> manyPull' pulls' (i - 1) (t : ts)


-- TODO: fix the random pull
pullTetromino :: Pulls -> IO (Pulls, Tetromino)
pullTetromino pulls = do

  let leastPulled = take (length tetrominoTypes `div` 2) (sortBy (\(_, a) (_, b) -> a `compare` b) pulls)
  i <- getStdRandom (randomR (0, length leastPulled - 1))

  let t = realTetromino $ fst (leastPulled !! i)

  return (updatePair (tetType t) (+ 1) pulls, t)

-- yeah i really strugled to think of a good data representation for this
realTetromino :: TetrominoType -> Tetromino
realTetromino TetrominoI = Tetromino [Mino (-3, 0) (1, 255, 255),   (Mino (-2, 0) (1, 255, 255)), Mino (-1, 0) (1, 255, 255), Mino (0, 0) (1, 255, 255)]  (-1, 0) TetrominoI
realTetromino TetrominoO = Tetromino [Mino (-1, -1) (255, 255, 0),  (Mino (0, -1) (255, 255, 0)), Mino (-1, 0) (255, 255, 0), Mino (0, 0) (255, 255, 0)]  (0, 0)  TetrominoO
realTetromino TetrominoT = Tetromino [Mino (-1, 0) (153, 0, 255),   (Mino (0, 0) (153, 0, 255)),  Mino (1, 0) (153, 0, 255),  Mino (0, -1) (153, 0, 255)] (0, 0)  TetrominoT
realTetromino TetrominoJ = Tetromino [Mino (-1, 0) (0, 0, 255),     (Mino (0, 0) (0, 0, 255)),    Mino (1, 0) (0, 0, 255),    Mino (-1, -1) (0, 0, 255)]  (0, 0)  TetrominoJ
realTetromino TetrominoL = Tetromino [Mino (-1, 0) (255, 170, 0),   (Mino (0, 0) (255, 170, 0)),  Mino (1, 0) (255, 170, 0),  Mino (1, -1) (255, 170, 0)] (0, 0)  TetrominoL
realTetromino TetrominoS = Tetromino [Mino (0, -1) (1, 255, 0),     (Mino (1, -1) (1, 255, 0)),   Mino (-1, 0) (1, 255, 0),   Mino (0, 0) (1, 255, 0)]    (0, 0)  TetrominoS
realTetromino TetrominoZ = Tetromino [Mino (-1, -1) (255, 1, 0),    (Mino (0, -1) (255, 1, 0)),   Mino (0, 0) (255, 1, 0),    Mino (1, 0) (255, 1, 0)]    (0, 0)  TetrominoZ

-- just a list of all of the tetromino types
tetrominoTypes :: [TetrominoType]
tetrominoTypes = [TetrominoI, TetrominoO, TetrominoT, TetrominoJ, TetrominoL, TetrominoS, TetrominoZ]


--               Tetromino  Body  Rotation Point  Type
data Tetromino = Tetromino [Mino] BoardPos2D      TetrominoType
  deriving ( Show )

mapBody :: (Mino -> Mino) -> Tetromino -> Tetromino
mapBody f (Tetromino bs r t) = Tetromino (map f bs) r t


filterBody :: (Mino -> Bool) -> Tetromino -> [Mino]
filterBody f (Tetromino bs _ _) = filter f bs


tetBody :: Tetromino -> [Mino]
tetBody (Tetromino b _ _) = b


mTetBody :: Maybe Tetromino -> Maybe [Mino]
mTetBody (Just (Tetromino b _ _)) = Just b
mTetBody Nothing                  = Nothing


tetType :: Tetromino -> TetrominoType
tetType (Tetromino _ _ t) = t


data Mino = Mino BoardPos2D (GLfloat, GLfloat, GLfloat)
  deriving ( Show )
instance Eq Mino where
  (==) (Mino p1 _) (Mino p2 _) = p1 == p2


-- pretty types:
type BoardPos2D = (Int, Int) -- position within the play area, not to be confused with display position (GLFloat, GLFloat) (simplified to Float's)
type Frame      = Integer
type Score      = Int
type Level      = Int
type Pulls      = [(TetrominoType, Int)]

data Rotation = ToLeft | ToRight
  deriving ( Show )

data TetrominoType
    = TetrominoI
    | TetrominoO
    | TetrominoT
    | TetrominoJ
    | TetrominoL
    | TetrominoS
    | TetrominoZ
  deriving ( Eq, Show )
