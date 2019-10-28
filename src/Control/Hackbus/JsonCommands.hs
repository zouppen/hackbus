{-# LANGUAGE OverloadedStrings #-}
module Control.Hackbus.JsonCommands where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map.Lazy as M

data Command = Read [T.Text] | Write (M.Map T.Text Value) deriving (Show)

data Answer = Wrote | Return (M.Map T.Text Value) | Failed String deriving (Show)

instance FromJSON Command where
  parseJSON = withObject "Commands" $ \v -> do
    method  <- v .: "method"
    case method of
      "r" -> Read <$> v .: "params"
      "w" -> Write <$> v .: "params"
      x -> fail $ "Unknown command: " ++ x

instance ToJSON Answer where
  toJSON Wrote      = object []
  toJSON (Return x) = object ["v" .= x]
  toJSON (Failed x) = object ["error" .= x]
