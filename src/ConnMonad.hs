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

class (Monad m) => NetworkMonad m where
        writeBuffer  :: ByteString -> m () -- TODO in NetworkMonad
        readBuffer   :: m ByteString

class (Log.Logger m, MonadSetting m, StreamMonad m, NetworkMonad m) => ConnMonad m where
        throwError   :: ErrorCode -> m a
        pushBackBuffer :: ByteString -> m ()
        runHandler :: StreamId -> Request -> m ()
        moreHeadersExpected :: m (Maybe StreamId)
        setMoreHeaders :: (Maybe StreamId) -> m ()
        sendFrame :: Frame -> m ()
        isStreamEnd :: m Bool
        setStreamEnd :: m ()
