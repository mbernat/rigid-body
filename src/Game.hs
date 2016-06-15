module Game
    ( game )
where

import Control.Concurrent
import Control.Monad
import SFML.Window.Keyboard

import Time
import Types


update :: Input -> State -> State
update input state = State pos'
  where
    pos' = pos state + mod
    k = keyboard input
    mod = if k KeyA then -1 else 1

game :: MVar Input -> MVar State -> IO ()
game input state = do
    i <- readMVar input
    modifyMVar_ state (pure . update i)
