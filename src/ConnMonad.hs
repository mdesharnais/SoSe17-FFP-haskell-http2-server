module ConnMonad 
   ( ConnMonad (..)
   ) where

import Data.ByteString.Lazy (ByteString)

import Logger as Log
import Settings
import Streams
import ProjectPrelude
import Handler


class (Log.Logger m, MonadSetting m, StreamMonad m) => ConnMonad m where
        writeBuffer  :: ByteString -> m ()
        readBuffer   :: m ByteString
        throwError   :: ErrorCode -> m a
        pushBackBuffer :: ByteString -> m ()
        runHandler :: Request -> m ()
        moreHeadersExpected :: m (Maybe StreamId)
        setMoreHeaders :: (Maybe StreamId) -> m ()

