module Time
    ( loop )
where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock

import Types


timeToMicro :: DTime -> Int
timeToMicro = round . (* 10^6)

step :: DTime -> IO a -> IO (Maybe DTime)
step delta comp = do
  begin <- getCurrentTime
  void comp
  end <- getCurrentTime
  let diff = diffUTCTime end begin
  if diff > delta then
    pure . Just $ diff - delta
  else do
    threadDelay . timeToMicro $ delta - diff
    pure Nothing

loop :: DTime -> IO a -> IO b
loop delta = forever . step delta
