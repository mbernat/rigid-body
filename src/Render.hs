module Render
    ( render )
where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import Foreign.C.Types
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import SDL.Video.Renderer
import SDL (($=))

import Time
import Types


makeRect :: (Num a) => Point V2 a -> a -> Rectangle a
makeRect (P (V2 x y)) h = Rectangle (P $ V2 (x - h) (y - h)) (V2 h h)

render :: Renderer -> MVar State -> IO ()
render renderer state = do
    (State x) <- readMVar state
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    let xx = CInt $ fromIntegral x
    rendererDrawColor renderer $= V4 255 255 255 255
    fillRect renderer $ Just $ makeRect (P (V2 xx 200)) 100

    present renderer

