{-# LANGUAGE ForeignFunctionInterface #-}

module Client.Client (render) where

import           Control.Applicative ((<$>))
import           Control.Monad       ((<=<))
import           Control.Monad.Loops (unfoldWhileM)
import           Data.Monoid         (mconcat)
import           Haste               hiding (click)
import           Haste.App           (Client, MonadIO, Remote, Server, addChild,
                                      alert, liftIO, newElem, newTextElem,
                                      onServer)
import           Haste.DOM           (Elem, setClass)
import           Haste.JSON          (decodeJSON)
import           Haste.Prim          (toJSStr)
import           Haste.Serialize     (Serialize, fromJSON)

import           Client.UI.Env       (setDeploy)
import           Types.DeployEvent
import Types.API (API(..))



foreign import ccall scrollDown :: IO ()

render :: API -> Elem -> Client ()
render api container = getPayload (getDeployChunk api) >>= either (addError container) (setDeploy api container)

  where
    getPayload :: Serialize a => Remote (Server String) -> Client (Either String a)
    getPayload getChunk = (fromJSON <=< decodeJSON . toJSStr . mconcat) <$> (unfoldWhileM (/="") $ onServer getChunk)

addError :: MonadIO m => Elem -> String -> m ()
addError envsContainer errorText = do
  err <- newElem "div"

  errText <- newTextElem errorText
  errText `addChild` err

  setClass err "alert" True
  setClass err "alert-danger" True
  err `addChild` envsContainer
