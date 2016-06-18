module Time
    ( loop )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock

import Types


timeToMicro :: DTime -> Int
timeToMicro = round . (* 10^6)

step :: MonadIO m => DTime -> m a -> m (Maybe DTime)
step delta comp = do
  begin <- liftIO getCurrentTime
  void comp
  end <- liftIO getCurrentTime
  let diff = diffUTCTime end begin
  if diff > delta then
    pure . Just $ diff - delta
  else do
    liftIO . threadDelay . timeToMicro $ delta - diff
    pure Nothing

loop :: MonadIO m => DTime -> m a -> m b
loop delta = forever . step delta
