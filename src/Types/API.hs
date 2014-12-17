module Types.API where

import           Haste.App (Client, Remote, Server)

data API = API {
    getDeployChunk     :: Remote (Server String)
  }

