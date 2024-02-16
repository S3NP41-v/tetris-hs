module Display ( display ) where

import Graphics.UI.GLUT
import Data.IORef
import Control.Concurrent ( threadDelay )

import State



display :: IORef GameState -> DisplayCallback
display gsRef = do
  clear [ColorBuffer] >> loadIdentity
  threadDelay $ floor ((1 / fromIntegral frameRate) * 10^6)
  
  gs <- readIORef gsRef

  -- outline of the play area
  playAreaOutline
  -- game state, score, next piece ETC
  gameStateDisplay gsRef
  -- the game board
  gameBoard gsRef

  gsRef $= gs{frame=frame gs + 1}
  swapBuffers


gameBoard :: IORef GameState -> IO ()
gameBoard gsRef = do
  gs <- readIORef gsRef

  -- Controlled Piece
  case currentTetromino gs of
    Nothing -> pure ()
    Just t  -> tetromino (blockScale, 0) t
  
  mapM_ (\(Mino (x, y) rgb) -> block (blockScale * (fromIntegral x + 1), blockScale * fromIntegral y) rgb) (unmovableTetrominos gs)


playAreaOutline :: IO ()
playAreaOutline = do
  -- gray blocks outline
  -- walls
  mapM_ (\i -> block (0,               fromIntegral yRes - (blockScale * i)) (31, 31, 31) 
            >> block (blockScale * 11, fromIntegral yRes - (blockScale * i)) (31, 31, 31)
        ) [0..20]

  -- floor
  mapM_ (\i -> block (blockScale * i, fromIntegral yRes) (31, 31, 31)) [1..10]


gameStateDisplay :: IORef GameState -> IO ()
gameStateDisplay gsRef = do
  gs <- readIORef gsRef

  -- Score display
  putText (12.5 * blockScale,   blockScale) (255, 255, 255) "SCORE"
  putText (12.5 * blockScale, 2*blockScale) (255, 255, 255) (prepend0s (show (score gs)))

  -- Next piece display
  putText (12.5 * blockScale, 4*blockScale) (255, 255, 255) "NEXT"
  case next gs of
    Nothing                           -> pure ()
    Just t@(Tetromino _ _ TetrominoI) -> tetromino (15.5 * blockScale, 6.5 * blockScale) t
    Just t                            -> tetromino (14 * blockScale, 6.5 * blockScale) t

  -- Stored piece display
  putText (12.5 * blockScale, 7.5*blockScale) (255, 255, 255) "STORED"
  case stored gs of
    Nothing -> pure ()
    Just t@(Tetromino _ _ TetrominoI) -> tetromino (15.5 * blockScale, 10 * blockScale) (realTetromino TetrominoI)
    Just (Tetromino _ _ t)                            -> tetromino (14 * blockScale, 10 * blockScale) (realTetromino t)

  where
    prepend0s :: String -> String
    prepend0s cs = replicate (6 - length cs) '0' ++ cs


tetromino :: (Float, Float) -> Tetromino -> IO ()
tetromino _      (Tetromino [] _ _)                         = pure ()
tetromino (x, y) (Tetromino ((Mino (x', y') rgb): ts) r t) = block (x + blockScale * fromIntegral x', y + blockScale * fromIntegral y') rgb
                                                           >> tetromino (x, y) (Tetromino ts r t)

block :: (Float, Float) -> (Float, Float, Float) -> IO ()
block (x, y) (r, g, b) =
  renderPrimitive Quads $ do
    -- shadow goes first because it will be layered over with the actual block
    -- shadow of the block
    color3i (r * 0.8) (g * 0.8) (b * 0.8)
    stVertex2f (x,              y              )
    stVertex2f (x,              y - blockScale )
    stVertex2f (x + blockScale, y - blockScale )
    stVertex2f (x + blockScale, y              )

    -- block
    color3i r g b
    stVertex2f (x              + blockShadow, y              - blockShadow )
    stVertex2f (x              + blockShadow, y - blockScale + blockShadow )
    stVertex2f (x + blockScale - blockShadow, y - blockScale + blockShadow )
    stVertex2f (x + blockScale - blockShadow, y              - blockShadow )


blockOutline :: (Float, Float) -> (Float, Float, Float) -> IO ()
blockOutline (x, y) (r, g, b) =
  renderPrimitive LineLoop $ do
    color3i r g b
    stVertex2f (x              + blockShadow, y              - blockShadow )
    stVertex2f (x              + blockShadow, y - blockScale + blockShadow )
    stVertex2f (x + blockScale - blockShadow, y - blockScale + blockShadow )
    stVertex2f (x + blockScale - blockShadow, y              - blockShadow )


vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f x y = vertex (Vertex3 (scrx x) (scry y) (0 :: GLfloat))

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y (z :: GLfloat)

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g (b :: GLfloat)

color3i :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3i r g b = color (Color3 (r/255) (g/255) (b/255 :: GLfloat))


putText :: (Float, Float) -> (Float, Float, Float) -> String -> IO ()
putText (x, y) (r, g, b) str = preservingMatrix $ do
  stVector2f (x, y)
  color3i r g b
  scale 0.0007 0.0005 (1.0 :: Double)
  renderString MonoRoman str


-- strict coordinate vector and vertex, no more of the relative to the middle bs
stVector2f :: (Float, Float) -> IO ()
stVector2f (x, y) = translate $ Vector3 (scrx x) (scry y) 0

stVertex2f :: (GLfloat, GLfloat) -> IO ()
stVertex2f (x, y) = vertex $ Vertex3 (scrx x) (scry y) 0


scrx :: Fractional a => a -> a
scrx x = 2 * x / fromIntegral xRes - 1.0

scry :: Fractional a => a -> a
scry y = 1.0 - 2 * y / fromIntegral yRes
