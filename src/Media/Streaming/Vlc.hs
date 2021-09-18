{-# LANGUAGE RecordWildCards #-}
module Media.Streaming.Vlc (Vlc, vlcStart, vlcCmd, vlcClose) where

import System.IO
import System.Process

data Vlc = Vlc { commandH :: Handle
               , nullH    :: Handle
               , procH    :: ProcessHandle
               }

-- |Start VLC with optional working directory and a list of files.
vlcStart :: Maybe FilePath -> [String] -> IO Vlc
vlcStart cwd files = do
  nullH <- openFile "/dev/null" WriteMode
  (Just commandH, _, _, procH) <- createProcess cp{ std_out = UseHandle nullH }
  return Vlc{..}
  where
    cp'    = proc "vlc" $ params ++ files
    cp     = cp'{ std_in = CreatePipe, cwd = cwd }
    params = ["-I","rc","--play-and-stop","--no-playlist-autostart","--no-dbus","--"]

-- |Runs given VLC command. See all supported commands by runnning:
-- `echo help | vlc -I rc`
vlcCmd :: Vlc -> String -> IO ()
vlcCmd Vlc{..} cmd = do
  hPutStrLn commandH cmd
  hFlush commandH

-- |Close VLC cleanly
vlcClose :: Vlc -> IO ()
vlcClose Vlc{..} = do
  -- Normally, VLC listens for EOF and handle closure ends the process
  hClose commandH
  waitForProcess procH
  hClose nullH
  return ()
