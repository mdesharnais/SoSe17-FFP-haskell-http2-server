module ConnMonad
   ( ConnMonad (..)
   , NetworkMonad (..)
   ) where

import Data.ByteString.Lazy (ByteString)

import Logger as Log
import Settings
import Streams
import ProjectPrelude
import Handler
import Frame (Frame)
import ErrorCodes
import Hpack

class (Log.Logger m, MonadSetting m, StreamMonad m, NetworkMonad m) => ConnMonad m where
        throwError   :: ConnError -> m a
        pushBackBuffer :: ByteString -> m ()
        runHandler :: StreamId -> Request -> m ()
        moreHeadersExpected :: m (Maybe StreamId)
        setMoreHeaders :: (Maybe StreamId) -> m ()
        sendFrame :: Frame -> m ()
        isConnEnd :: m Bool
        setConnEnd :: m ()
        getDynamicTable :: Endpoint -> m DynamicTable
        setDynamicTable :: Endpoint -> DynamicTable -> m ()

class (Monad m) => NetworkMonad m where
        writeBuffer  :: ByteString -> m ()
        readBuffer   :: m ByteString
