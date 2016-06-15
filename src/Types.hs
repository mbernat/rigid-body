module Types
    ( DTime
    , Input(..)
    , State(..)
    )
where

import Data.Time.Clock
import SFML.Window.Keyboard (KeyCode)


data State = State
    { pos :: Int
    }

data Input = Input
    { keyboard :: KeyCode -> Bool
    }

type DTime = NominalDiffTime
