module Handle.RSTStream
  ( handleRSTStream
  , sendRstStream
  ) where

import Data.String (fromString)

import qualified Frame.RSTStream as FRSTStream
import ProjectPrelude
import ErrorCodes
import ConnMonad
import qualified Logger as Log
import Streams
import Frame (Frame (..), Payload(PRSTStream))

handleRSTStream :: (ConnMonad m) => FRSTStream.Payload -> StreamId -> FrameFlags -> m ()
handleRSTStream payload (StreamId sid) _flags = do
             Log.log Log.Crit $ fromString $ "Stream " ++ show sid ++ " canceled due to " ++ show payload
             setStreamState (StreamId sid) $ StreamRst RemoteEndpoint

sendRstStream :: (ConnMonad m) => StreamId -> ErrorCode -> m ()
sendRstStream sid errcode = do
           setStreamState sid $ StreamRst LocalEndpoint
           let fFlags = 0
               fStreamId = sid
               fPayload = PRSTStream errcode
           sendFrame $ Frame { fPayload, fStreamId, fFlags }

