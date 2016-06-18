{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Render
    ( run )
where

import Control.Concurrent
import Control.Monad

import Control.Monad.IO.Class
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Graphics.GPipe.Context.GLFW.Input

import Time
import Types


run :: MVar Input -> MVar State -> IO ()
run input state = runContextT GLFW.newContext (ContextFormatColor RGB8) $ do
    vertexBuffer :: VertexBuffer os <- newBuffer 3
    writeBuffer vertexBuffer 0 [ (V4 (-1) 1 0 1, V3 1 0 0)
                               , (V4 0 (-1) 0 1, V3 0 1 0)
                               , (V4 1 1 0 1,  V3 0 0 1)
                               ]

    shader <- compileShader $ do
        primitiveStream <- toPrimitiveStream id
        fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream
        drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop renderDelta $ step' vertexBuffer shader input state
  where
    renderDelta = 0.01 -- 100 FPS

step'
    :: (f ~ (ContextFormat RGBFloat d))
    => VertexBuffer os
    -> CompiledShader os f Environment
    -> MVar Input
    -> MVar State
    -> ContextT GLFW.GLFWWindow os f IO ()
step' vertexBuffer shader input state = do
    s <- liftIO $ readMVar state
    render $ do
        clearContextColor (V3 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
    swapContextBuffers

    keyState <- getKey Key'A
    let pressed = keyState == KeyState'Pressed
    liftIO $ print pressed
    liftIO $ void . swapMVar input $ Input $ \key -> (key == Key'A) && pressed

    -- TODO keep this in state
    -- closeRequested <- GLFW.windowShouldClose
