module ServerConfig 
     ( ServerConfig (..)
     , ServerError (..)
     ) where

import ProjectPrelude
import Handler

data ServerConfig = ServerConfig 
              { servHostname :: String
              , servPort :: Word16
              , servHandler :: Handler
                -- TODO Ueberlegen coustom Logger in config
              }

data ServerError = HostNotFound | BindFaild deriving Show
