module Types
    ( DTime
    , Input(..)
    , State(..)
    )
where

import Data.Time.Clock
import SDL.Input (Scancode)


data State = State
    { pos :: Int
    }

data Input = Input
    { keyboard :: Scancode -> Bool
    }

type DTime = NominalDiffTime
