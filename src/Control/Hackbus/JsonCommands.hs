{-# LANGUAGE OverloadedStrings #-}
module Control.Hackbus.JsonCommands where

import Data.Aeson
import qualified Data.Text as T

data Command = Read T.Text | Write T.Text Value deriving (Show)

data Answer = Wrote | Return Value | Failed String deriving (Show)

instance FromJSON Command where
  parseJSON = withObject "Command" $ \v -> do
    r <- v .:? "r"
    w <- v .:? "w"
    case (r,w) of
      (Just x, Nothing) -> pure $ Read x
      (Nothing, Just x) -> Write x <$> v .: "v"
      _                 -> fail "Both r and w defined"

instance ToJSON Answer where
  toJSON Wrote      = object []
  toJSON (Return x) = object ["v" .= x]
  toJSON (Failed x) = object ["error" .= x]
