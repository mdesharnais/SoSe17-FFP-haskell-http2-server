module LoggerImpl() where

import Control.Monad.IO.Class (liftIO)

import qualified Logger as Log
import ConnectionM
import ProjectPrelude

instance Log.Logger (ConnectionM mode) where
      log = liftIO `oo` Log.log
