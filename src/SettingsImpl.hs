module SettingsImpl () where

import qualified Data.Map.Strict as Map
import Control.Monad.RWS
import Control.Concurrent.STM

import Settings
import Streams
import ConnectionM
import ProjectPrelude

instance MonadSetting (ConnectionM mode) where
       getSettings = do
               settings <- asks stSettings
               liftIO $ readTVarIO settings
       getSetting f = do
               settings <- asks stSettings 
               liftIO $ fmap f $ readTVarIO settings
       modifySettings f = do
               settings <- asks stSettings
               liftIO $ atomically $ modifyTVar settings f
       -- adjustRemoteWindowSize :: Int64 -> Word32 -> m ()
       adjustRemoteWindowSize diff maxSize = do
               let maxSize' = fromIntegral maxSize
                   changeVal oldWin = min (oldWin + diff) maxSize'
                   changeStream sid streamData = when (sid /= StreamId 0) $
                          liftIO $ atomically $ modifyTVar (sendWindow streamData) changeVal
               connSendWinV <- asks stConnSendWindow
               liftIO $ atomically $ modifyTVar connSendWinV changeVal
               streams <- gets stStreams
               sequence_ $ Map.mapWithKey changeStream streams
