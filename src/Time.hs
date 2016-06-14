module Time
    ( step )
where

import Control.Concurrent
import Data.Time.Clock

import Types


timeToMicro :: DTime -> Int
timeToMicro = round . (* 10^6)

step :: DTime -> IO () -> IO (Maybe DTime)
step delta comp = do
  begin <- getCurrentTime
  comp
  end <- getCurrentTime
  let diff = diffUTCTime end begin
  if diff > delta then
    pure . Just $ diff - delta
  else do
    threadDelay . timeToMicro $ delta - diff
    pure Nothing

