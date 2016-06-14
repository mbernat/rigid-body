module Game
    ( game )
where

import Control.Concurrent
import Control.Monad

import Time
import Types
  

update :: State -> State
update (State x) = State $ x+1
             
game :: DTime -> MVar State -> IO ()
game delta state = forever . step delta $ do 
    modifyMVar_ state (pure . update)

    
