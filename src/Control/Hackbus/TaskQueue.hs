module Control.Hackbus.TaskQueue where

import Data.Time.Clock.POSIX
import Data.PQueue.Min

data Task = Task { runAt :: POSIXTime
                 , task  :: POSIXTime -> IO (Maybe POSIXTime)
                 }

instance Eq Task where
  Task a _ == Task b _ = a == b

instance Ord Task where
  compare (Task a _) (Task b _) = compare a b

-- |Run task once.
once :: IO a -> Task
once act = Task 0 $ \_ -> act >> pure Nothing

-- |Run task periodically. Interval is calculated between end and start
-- time so timer jitter and execution time makes the timer lag. In
-- most cases this is desired.
repeatedCoarse :: POSIXTime -> IO a -> Task
repeatedCoarse interval act = Task 0 $ \target -> do
  act
  now <- getPOSIXTime
  pure $ Just $ now + interval

-- |Run task periodically and precisely. Intervals are calculated from
-- the first execution time so jitter doesn't accumulate. If for any
-- reason the timer to miss the target, then this timer is run
-- multiple times to catch up with the clock. Most likely too
-- aggressive for you.
repeatedFine :: POSIXTime -> IO a -> Task
repeatedFine interval act = Task 0 $ \target -> do
  act
  if target == 0
    then do now <- getPOSIXTime
            pure $ Just $ now + interval
    else pure $ Just $ target + interval

-- |Run task periodically and precisely but allow "frameskip".
-- Intervals are calculated from the first execution time so jitter
-- doesn't accumulate. If for any reason the timer to miss the target
-- the timer is scheduled for the next multiple of interval from the
-- first execution time.
repeatedFineSkip :: POSIXTime -> IO a -> Task
repeatedFineSkip interval act = Task 0 f
  where
    f 0 = do
      start <- getPOSIXTime
      act
      pure $ Just $ start + interval
    f target = do
      act
      now <- getPOSIXTime
      pure $ Just $ target + interval * fromIntegral (ceiling $ (now - target) / interval)
