{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import SDL.Video (createWindow, defaultWindow, createRenderer)
import SDL.Video.Renderer
import SDL.Event
import SDL.Init (initializeAll)
import SDL.Input
import SDL (($=))
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import Control.Monad (unless)

main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer

makeRect :: (Num a) => Point V2 a -> a -> Rectangle a
makeRect (P (V2 x y)) h = Rectangle (P $ V2 (x - h) (y - h)) (V2 h h)

appLoop :: Renderer -> IO ()
appLoop renderer = do
    events <- pollEvents
    let eventIsQPress event =
            case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                _ -> False
        qPressed = not (null (filter eventIsQPress events))

    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    rendererDrawColor renderer $= V4 255 255 255 255
    fillRect renderer $ Just $ makeRect (P (V2 200 200)) 100

    present renderer
    unless qPressed (appLoop renderer)
