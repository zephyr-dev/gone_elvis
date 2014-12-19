{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Haste.App                 (liftIO, mkConfig, remote,
                                            runApp, runClient,
                                            withElem)
#ifndef __HASTE__

import           Control.Monad             (forM_, forever)

import           Server.Deploy             (startDeployServer)

#endif

import           Control.Applicative       ((<$>))
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, putMVar,
                                            takeMVar)
import           Data.List.Split           (chunksOf)
import           Haste.App                 (MonadIO)
{- import           Haste.App                 (MonadIO, forkServerIO) -}
import           Haste.JSON                (encodeJSON)
import           Haste.Prim                (fromJSStr)
import           Haste.Serialize           (toJSON)

import           Client.Client             (render)
import           Types.DeployEvent
import           Types.API                 (API(..))

maxStringLength = 2048 :: Int

#ifdef __HASTE__

getDeploy = undefined

#else

getDeploy :: MonadIO m => MVar String -> m String
getDeploy = liftIO . takeMVar

deployPump :: MVar DeployEvent -> MVar String -> IO ()
deployPump reqs deployChunks = forever $ do
  reqS <- (fromJSStr . encodeJSON . toJSON) <$> takeMVar reqs
  forM_ (chunksOf maxStringLength reqS) $ putMVar deployChunks
  putMVar deployChunks ""

#endif


main :: IO ()
main = do
  deploys        <- newEmptyMVar
  deployChunks   <- newEmptyMVar
  
#ifndef __HASTE__
  forkIO $ startDeployServer deploys
  forkIO $ deployPump deploys deployChunks
#endif

  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    api <- API  <$> remote (getDeploy deployChunks)

    runClient $ withElem "envs" $ render api

