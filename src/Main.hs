{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import Control.Monad
import SDL.Video (createWindow, defaultWindow, createRenderer)
import SDL.Video.Renderer
import SDL.Event
import SDL.Init (initializeAll)
import SDL.Input

import Types
import Render
import Game              
       

main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    
    state <- newMVar $ State 10
    void . forkIO $ render renderDelta renderer state
    void . forkIO $ game gameDelta state
    threadDelay $ 10^7
  where
    renderDelta = 0.01 -- 100 FPS
    gameDelta = 0.001  -- 1000 FPS


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



Note: I'm not sure how FRP is supposed to help us here.
However, I can see how a SAC could save a lot of recomputation in
rendering (a la React) and perhaps in game stepping too.

-}

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

