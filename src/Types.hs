module Types
    ( DTime
    , Environment
    , Input(..)
    , State(..)
    , VertexBuffer
    )
where

import Data.Time.Clock
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW.Input


type VertexBuffer os = Buffer os (B4 Float, B3 Float)
type Environment = PrimitiveArray Triangles (B4 Float, B3 Float)

data State = State
    { pos :: Int
    }

data Input = Input
    { keyboard :: Key -> Bool
    }

type DTime = NominalDiffTime
