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
    s <- readMVar state
    print (pos s)
    print (vel s)

    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    let (V2 x y) = pos s
    let toCInt = CInt . fromIntegral . round
    let iPos = fmap toCInt (pos s)
    rendererDrawColor renderer $= V4 255 255 255 255
    fillRect renderer $ Just $ makeRect (P iPos) 100

    present renderer

