{-# LANGUAGE RecordWildCards #-}
module Physics
    ( Collisions(..)
    , Force(..)
    , Forces(..)
    , Particle(..)
    , World(..)
    , bigStep
    , smallStep
    )
where

import Linear
import Linear.Affine


{- TODO
Contemplate the relationship of particles and forces.
Should they be included in the same data structure or kept separate?
-}

data World a = World
    { particles :: [Particle a]
    }

data Particle a = Particle
    { pos :: Point V2 a
    , vel :: V2 a
    , forces :: [Force a]
    }

data Force a = Force
    { place :: V2 a
    -- ^ Place of action of the force, relative to the particle it is acting on.

    , force :: V2 a
    -- ^ Direction and magnitude of the force.
    }

data Forces a = Forces
    { ambient :: [V2 a]
    }

data Collisions a where

{-

For now step is simple but as soon as we want to handle collisions
it will have to accept a callback to setup additional reactionary forces.

Such a generalized step will have the following algorithm:

1. Detect a collision
2a. If there is no collision then terminate with new state
2b. In case of a collision use the given callback to update forces (how?)
    and go to 1.

Also, each big step needs to consist of many small steps because of two reasons.
1. Improved integration accuracy.
2. To reduce a chance of objects passing through each other when going too fast
   in case a collision happens in-between the start and end time of the step.

Finally, in case a collision is detected a bisection needs to be used
to pin-point the earliest exact collision time.

----

Alternative design:
Each big step will do one of two things:
1. complete the full simulation for the given timestamp; or
2. return the full collision information in case a collision happened

It's then up to the caller to handle the collision and call the big step
simulation with updated forces.

-}

-- | This function simply integrates the forces against the world in the given
-- time. As such it is a basic building block of the engine.
smallStep :: Fractional a => Forces a -> a -> World a -> World a
smallStep fs dt World{..} = World
    { particles = map updateParticle particles
    }
  where
    updateParticle particle@Particle{..} = particle
        { pos = pos'
        , vel = vel'
        }
      where
        pos' = pos .+^ ((vel + vel') ^/ 2.0) ^* dt
        vel' = vel + totalForce ^* dt
        totalForce = ambientForces + relativeForces
        ambientForces = sum $ ambient fs
        relativeForces = sum $ map force forces


{- TODO
figure out a correct way of determining timescales for big and small steps
-}

smallTimeScale :: Fractional a => a
smallTimeScale = 10^^(-3)

-- | This function performs the simulation many times
-- using smaller timescale to improve the integration accuracy.
--
-- NOTE that it does not handle collisions for now,
-- so the resulting value is always @Right World@.
bigStep
    :: (Fractional a, Ord a)
    => Forces a
    -> a
    -> World a
    -> Either (a, Collisions a) (World a)
bigStep fs dt w =
    if  dt < smallTimeScale then
        Right next
    else
        bigStep fs (dt - smallTimeScale) next
  where
    next = smallStep fs dt w

