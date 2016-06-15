{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import Control.Monad

import SFML.Graphics
import SFML.Window

import Time
import Types
import Render
import Game


main :: IO ()
main = do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] ctxSettings

    state <- newMVar $ State 10
    input <- newMVar $ Input (const False)

    run renderDelta $ render wnd state
    run gameDelta $ game input state
    void . loop inputDelta $ readInput input
  where
    run delta = void . forkIO . loop delta
    renderDelta = 0.01 -- 100 FPS
    gameDelta = 0.001  -- 1000 FPS
    inputDelta = 0.01 -- 100 FPS

readInput :: MVar Input -> IO ()
readInput input = do
    pressed <- isKeyPressed KeyA
    print pressed
    void . swapMVar input $ Input $ \key -> (key == KeyA) && pressed
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

{-
getInput :: IO Input
getInput  = do
    events <- pollEvents
    let eventIsQPress event =
            case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed &&
                    keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events
    return $ Input qPressed
-}
