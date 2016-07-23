{-# LANGUAGE LambdaCase, RecordWildCards #-}
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
    ScancodeSpace -> V2 0 (-0.5)
    _ -> V2 0 0

{-

Physics looks as follows:

1. step velocities according to forces:
  * ambient (gravity/drag)
  * point-based (move/jump)
  * collision-based
  * epsilon cut-off
2. step positions according to new velocities
   thanks to the collision detection, this should be simple integration

Even better approach:

0. detect collisions from state
1. setup all forces (ambient, input-based, collision-based)
2. integrate velocities from forces
3. integrate positions from velocities
4. detect collisions & goto 1

This means that collisions would become part of state
...which is nice because they can be derived from it directly.
E.g. when two objects are in contact then the collision information
is just a simple calculation based on their positions and velocities.

-}

-- TODO timesteps
-- This is also necessary to handle collisions properly using bisection
-- to find the exact time of the first collision.

update :: Input -> Particle -> Particle
update input Particle{..} = Particle
    { pos = pos + vel'
    , vel = vel'
    }
  where
    vel''@(V2 vx' vy') = if ground then
              if falling
              then bounceVel
              else groundVel
            else airVel
    vel' = if abs vx' + abs vy' < 0.001 then 0 else vel''

    groundVel = V2 (vx * drag) vy + modVel
    airVel = V2 vx (vy + gravity)
    bounceVel = V2 vx (- vy * 0.5)

    modVel = Map.foldlWithKey plusVel (V2 0 0) keys
    plusVel move key _ = move + codeMove key

    ground = y >= 300
    slow = abs vy + abs vx < 0.005
    falling = vy > 0.001
    gravity = 0.003
    drag = 0.95

    keys = keyboard input
    (V2 x y) = pos
    (V2 vx vy) = vel


game :: MVar Input -> MVar State -> IO ()
game input state = do
    i <- readMVar input
    s <- takeMVar state
    let np = map (update i) (particles s)
    putMVar state s{particles = np}
