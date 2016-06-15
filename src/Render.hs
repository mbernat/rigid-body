module Render
    ( render )
where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import Foreign.C.Types
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import SFML.Graphics
import SFML.System

import Time
import Types


render :: RenderWindow -> MVar State -> IO ()
render wnd state = do
    (State x) <- readMVar state
    clearRenderWindow wnd black

    rect <- err createRectangleShape
    setSize rect (Vec2f 100 100)
    setPosition rect (Vec2f (fromIntegral x) 300)
    setFillColor rect white

    drawRectangle wnd rect Nothing
    display wnd
