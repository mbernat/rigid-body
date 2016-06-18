{-# LANGUAGE LambdaCase #-}
module Game
    ( game )
where

import Control.Concurrent
import Control.Monad
import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear
import SDL.Input.Keyboard

import Time
import Types


pressed :: Scancode -> Map Scancode () -> Bool
pressed k m = isJust $ Map.lookup k m

codeMove :: Scancode -> V2 Float
codeMove = \case
    ScancodeA -> V2 (-0.05) 0
    ScancodeD -> V2 (0.05) 0
    ScancodeSpace -> V2 0 (-0.3)
    _ -> V2 0 0

{-

correct physics would look as follows:

0. keys mean acceleration when the player is on the ground (input)
1. always step position and velocity (simulation)
2. fall leads to a bounce (collision detection)
3. stop totally when slow and underground (error correction)

-}

update :: Input -> State -> State
update input state =
    if ground
       then if falling then bounceState
            else stepGroundState
    else stepAirState
  where
    stepGroundState = State
        { pos = if slow then V2 x 300 else pos state + vel state
        , vel = V2 (vx * drag) (vy + gravity)
          + modVel
        }

    stepAirState = State
        { pos = pos state + vel state
        , vel = V2 vx (vy + gravity) -- gravity
        }

    bounceState = State
        { pos = pos state
        , vel = V2 vx (- vy * 0.5)
        }

    modVel = Map.foldlWithKey plusVel (V2 0 0) keys
    plusVel move key _ = move + codeMove key

    ground = y > 300
    slow = abs vy + abs vx < 0.001
    falling = vy > 0.01
    gravity = 0.003
    drag = 0.95

    keys = keyboard input
    (V2 x y) = pos state
    (V2 vx vy) = vel state

game :: MVar Input -> MVar State -> IO ()
game input state = do
    i <- readMVar input
    modifyMVar_ state (pure . update i)
