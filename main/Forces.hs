{-# LANGUAGE RecordWildCards #-}
module Forces
    ( forces
    )
where

import Control.Concurrent
import Linear.Affine

import RigidBody.Physics hiding (forces)
import qualified RigidBody.Physics as Physics
import qualified Types


update :: Types.Input -> Types.State -> Types.State
update i s = Types.State
    { particles = np
    }
  where
    np = either (const $ Types.particles s) (map stateParticle . particles) next
    next = bigStep (Forces [0.1]) 0.01 world
    world = World
        { particles = map (mkParticle $ Types.particles s) (Types.particles s)
        }

mkParticle :: [Types.Particle] -> Types.Particle -> Particle Float
mkParticle particles Types.Particle{..} = Particle
    { mass = 1
    , pos = P pos
    , vel = vel
    , Physics.forces = []
    }

stateParticle :: Particle Float -> Types.Particle
stateParticle Particle{..} = Types.Particle
    { Types.pos = pos .-. 0
    , Types.vel = vel
    }

forces :: MVar Types.Input -> MVar Types.State -> IO ()
forces input state = do
    i <- readMVar input
    modifyMVar_ state (pure . update i)
