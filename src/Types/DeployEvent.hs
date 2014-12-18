{-# LANGUAGE OverloadedStrings #-}

module Types.DeployEvent where

import           Control.Applicative ((<$>), (<*>))
import           Haste.JSON
import           Haste.Serialize

data DeployEvent = DeployEvent {
    env :: String
  , sha :: String
  }

instance Serialize DeployEvent where
  toJSON (DeployEvent env sha) = Dict [
      ("env",        toJSON env)
    , ("sha",        toJSON sha)
    ]

  parseJSON j =
    DeployEvent <$>
        (j .: "env")
    <*> (j .: "sha")
