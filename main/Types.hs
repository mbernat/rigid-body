module Types
    ( DTime
    , Input(..)
    , State(..)
    , Particle(..)
    )
where

import Data.Map.Strict
import Data.Time.Clock
import Linear
import SDL.Input (Scancode)


data State = State
    { particles :: [Particle]
    }

data Particle = Particle
    { pos :: V2 Float
    , vel :: V2 Float
    }

data Input = Input
    { keyboard :: Map Scancode ()
    }

type DTime = NominalDiffTime
