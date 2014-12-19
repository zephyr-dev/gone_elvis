{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Deploy where

import           Control.Concurrent.MVar    (MVar (..), putMVar)
import           Control.Monad
import           Data.Aeson                 (eitherDecode)
import           Control.Monad.IO.Class     (liftIO)

import Web.Scotty


{- import qualified Data.ByteString.Lazy.Char8 as BL -}
{- import qualified Data.Text                  as T -}

import Types.DeployEvent

startDeployServer :: MVar DeployEvent -> IO ()
startDeployServer deploys = do
  {- putMVar deploys $ DeployEvent "hotel" "deadbeef" -}
  scotty 5000 $ do
    post "/deploy" $ do
      liftIO $ putMVar deploys $ DeployEvent "hotel" "deadbeef"
      html $ "yo!"

