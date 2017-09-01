module Handle.Goaway 
  ( handleGoaway
  , sendGoaway
  ) where

import Data.ByteString.Lazy (ByteString)

import qualified Frame.Goaway as FGoaway
import ProjectPrelude
import ErrorCodes
import ConnMonad

handleGoaway :: (ConnMonad m) => FGoaway.Payload -> StreamId -> FrameFlags -> m ()
handleGoaway = undefined 

sendGoaway :: (ConnMonad m) => ErrorCode -> ByteString -> m ()
sendGoaway = undefined
