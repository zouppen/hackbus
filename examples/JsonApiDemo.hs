{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM
import Control.Hackbus.UnixJsonInterface
import Data.Map.Lazy (fromList)
import System.Environment (getArgs)
import Data.Text (Text)

main = do
  -- Minimal command line parsing.
  args <- getArgs
  unixPath <- case args of
    [a] -> return a
    _   -> fail "Give unix socket name"

  -- Declare some variables and publish them
  luku <- newTVarIO (13::Integer)
  let tupla = (\x -> x*2-1) <$> readTVar luku
  lemmikki <- newTVarIO ("kissa"::String)
  vakio <- newTVarIO ("I'm immutable"::Text)
  
  let m = fromList [("luku", readwrite luku)
                   ,("tupla", readAction tupla)
                   ,("lemmikki", readwrite lemmikki)
                   ,("vakio", readonly vakio)
                   ]

  listenJsonQueries m unixPath
