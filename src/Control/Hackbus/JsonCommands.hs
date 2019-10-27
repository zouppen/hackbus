{-# LANGUAGE OverloadedStrings #-}
module Control.Hackbus.JsonCommands where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map.Lazy as M

data Command = Read T.Text | Write T.Text Value deriving (Show)

data Answer = Wrote | Return Value | Failed String deriving (Show)

instance FromJSON Command where
  parseJSON = withObject "Command" $ \v -> do
    method  <- v .: "method"
    case method of
      "r" -> Read <$> v .: "params"
      "w" -> do
        params <- v .: "params"
        case M.toList params of
          [(k,v)] -> return $ Write k v
          _ -> fail "Multiple values not supported yet"
      x -> fail $ "Unknown command: " ++ x

instance ToJSON Answer where
  toJSON Wrote      = object []
  toJSON (Return x) = object ["v" .= x]
  toJSON (Failed x) = object ["error" .= x]
