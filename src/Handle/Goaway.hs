{-# LANGUAGE NamedFieldPuns #-}
module Handle.Goaway
  ( handleGoaway
  , sendGoaway
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)
import Control.Monad (when)

import qualified Frame.Goaway as FGoaway
import ProjectPrelude
import ErrorCodes
import ConnMonad
import qualified Logger
import Frame (Frame (..), Payload(PGoaway))

handleGoaway :: (ConnMonad m) => FGoaway.Payload -> StreamId -> FrameFlags -> m ()
handleGoaway payload sid _flags = do
         when (sid /= StreamId 0) $ throwError $ ConnError ConnectionError ProtocolError
         when (FGoaway.getErrorCode payload /= NoError) $ do
              let errCode = FGoaway.getErrorCode payload
              Logger.log Logger.Crit $ fromString $ "Goaway frame resived: ErrorCode : " ++ show errCode
                     ++ " debug message: " ++ show (FGoaway.getDebugData payload)
         setConnEnd
         throwError $ ConnError ConnectionError EndOfConn

sendGoaway :: (ConnMonad m) => ErrorCode -> ByteString -> m ()
sendGoaway errCode debugData = do
      let fFlags = 0
          fStreamId = StreamId 0
          fPayload = PGoaway $ FGoaway.mkPayload errCode debugData
      sendFrame $ Frame { fPayload, fStreamId, fFlags }
