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

renderParticle :: Renderer -> Particle -> IO ()
renderParticle renderer p = do
    let (V2 x y) = pos p
    let toCInt = CInt . fromIntegral . round
    let iPos = fmap toCInt (pos p)
    rendererDrawColor renderer $= V4 255 255 255 255
    fillRect renderer $ Just $ makeRect (P iPos) 100

render :: Renderer -> MVar State -> IO ()
render renderer state = do
    ps <- readMVar state

    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    mapM_ (renderParticle renderer) (particles ps)

    present renderer

