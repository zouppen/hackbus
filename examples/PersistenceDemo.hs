{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Hackbus.Persistence

main = withPersistence 60 "/tmp/persistence-test.json" $ \pers -> do
  var <- atomically $ newTVarPers pers "counter" 0
  forever $ join $ atomically $ do
    now <- readTVar var
    writeTVar var (now+1)
    pure $ do
      putStrLn $ "Counter is " ++ show (now :: Int)
      threadDelay 1000000
