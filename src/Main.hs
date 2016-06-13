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
import Data.Functor.Identity
import Control.Wire.Core

data Input = Input Bool
data State = State Bool

-- Wire s e m a b

{-
Inside wire, potentially over a (non-identity) monad.

step :: (State, Input) -> State
render :: State -> Output (e.g. diagrams)

Outside the wire.

getInput :: IO Input
writeOutput :: Output -> IO ()

Would it make sense to put IO into Wire too?
No idea right now, gut feeling is no.

-}

type W s = Wire s () Identity

wire :: W s Input State
wire = mkPure_ $ \(Input x) -> Right $ State x 

main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    loop wire renderer (Just $ Input False)

getWire :: W s a b -> s -> Either () a -> (Either () b, W s a b)
getWire w dt x = runIdentity $ stepWire w dt x

loop :: W Float Input State -> Renderer -> Maybe Input -> IO ()
loop w renderer init = do
    i <- maybe getInput return init
    let (o, w') = getWire wire delta (Right i)
    either
        (const $ return ())
        (\s -> render renderer s >> loop w' renderer Nothing)
        o
        
  where
    delta = 0.1

makeRect :: (Num a) => Point V2 a -> a -> Rectangle a
makeRect (P (V2 x y)) h = Rectangle (P $ V2 (x - h) (y - h)) (V2 h h)

getInput :: IO Input
getInput  = do
    events <- pollEvents
    let eventIsQPress event =
            case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                _ -> False
        qPressed = not (null (filter eventIsQPress events))
    return $ Input qPressed

render :: Renderer -> State -> IO ()
render renderer _state = do
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer

    rendererDrawColor renderer $= V4 255 255 255 255
    fillRect renderer $ Just $ makeRect (P (V2 200 200)) 100

    present renderer
