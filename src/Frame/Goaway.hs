module Frame.Goaway
   ( getPayload
   , putPayload
   , Payload
   , mkPayload
   , getErrorCode
   , getDebugData
   ) where

import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Trans (lift)
import qualified Data.Bits as Bits
import Data.ByteString.Lazy (ByteString)

import ProjectPrelude
import ErrorCodes

data Payload = Payload { lastStreamId :: StreamId
                       , errorCode :: ErrorCode
                       , debugData :: ByteString
                       }

mkPayload :: ErrorCode -> ByteString -> Payload
mkPayload errorCode debugData = Payload { lastStreamId = StreamId 1, errorCode, debugData }

getErrorCode :: Payload -> ErrorCode
getErrorCode = errorCode

getDebugData :: Payload -> ByteString
getDebugData = debugData

getPayload :: FrameLength -> FrameFlags -> StreamId -> ExceptT ConnError Get Payload
getPayload fLength _ _ = 
  if fLength < 8 then
    Except.throwError $ ConnError ConnectionError FrameSizeError
  else do
    lastSid <- lift $ flip Bits.clearBit 31 <$> Get.getWord32be
    errorCode <- lift $ errorCodeFromWord32 <$> Get.getWord32be
    debugData <- lift $ Get.getLazyByteString (fromIntegral $ fLength - 8)
    return $ Payload { lastStreamId = StreamId lastSid, errorCode, debugData }
     

putPayload :: Payload -> Put
putPayload (Payload {lastStreamId = StreamId lastSid, errorCode, debugData }) = do
                Put.putWord32be lastSid
                Put.putWord32be $ errorCodeToWord32 errorCode
                Put.putLazyByteString debugData

-- toString :: String -> Payload -> String
-- toString prefix increment = prefix ++ show increment
