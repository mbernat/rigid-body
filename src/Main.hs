{-# LANGUAGE OverloadedStrings #-}
module Main (main) where



import Control.Concurrent
import Control.Monad

import qualified Data.Map.Strict as Map
import Linear
import SDL.Video (createWindow, defaultWindow, createRenderer)
import SDL.Video.Renderer
import SDL.Event
import SDL.Init (initializeAll)
import SDL.Input

import Time
import Types
import Render
import Game


main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer

    state <- newMVar $ State 300 0
    input <- newMVar $ Input Map.empty

    run renderDelta $ render renderer state
    run gameDelta $ game input state
    void . loop inputDelta $ pollInput input
  where
    run delta = void . forkIO . loop delta
    renderDelta = 0.01 -- 100 FPS
    gameDelta = 0.001  -- 1000 FPS
    inputDelta = 0.001 -- 100 FPS

updateInput :: Event -> Input -> Input
updateInput event input =
    case eventPayload event of
        KeyboardEvent keyboardEvent ->
            Input $ case keyboardEventKeyMotion keyboardEvent of
                Pressed -> Map.insert scanCode () keys
                Released -> Map.delete scanCode keys
              where scanCode = keysymScancode $ keyboardEventKeysym keyboardEvent
        _ -> input
  where
    keys = keyboard input

pollInput :: MVar Input -> IO ()
pollInput input = do
    events <- pollEvents
    let update = foldl comp id events
    modifyMVar_ input (return . update)
  where
    comp up ev = updateInput ev . up
{-

Architecture:

1. Game state.
The basic idea is that there's a function updating the game state.
This function will run in its own thread at a certain speed (meaning
the sample rate of the state).
Regarding the updates, they will happen by means of passage of time
and user input. Let's call it a controlled simulation.

2. Rendering
Another function will take care of rendering the state (either directly
or to something like diagrams, which will be subsequently passed to
the actual backend). This function will run in another thread and
another rate and will simply pick whatever state is currently available.

3. Input
a) listen to events
b) poll state


Note: I'm not sure how FRP is supposed to help us here.
However, I can see how a SAC could save a lot of recomputation in
rendering (a la React) and perhaps in game stepping too.

-}

