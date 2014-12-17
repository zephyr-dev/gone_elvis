module Types.DeployEvent where

data DeployEvent = DeployEvent {
    env :: String
  , sha :: String
  }
