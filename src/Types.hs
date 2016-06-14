module Types
    ( DTime
    , Input(..)
    , State(..)
    )
where

import Data.Time.Clock


data State = State Int

data Input = Input Bool

type DTime = NominalDiffTime
