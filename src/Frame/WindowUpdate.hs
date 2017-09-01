module Frame.WindowUpdate(
  Payload,
  getPayload,
  putPayload,
  toString
) where

import qualified Control.Monad.Except as Except
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.Bits as Bits

import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)

import ProjectPrelude
import ErrorCodes

type Payload = Word32

getPayload :: FrameLength -> FrameFlags -> StreamId -> ExceptT ErrorCode Get Payload
getPayload fLength _ _ =
  if fLength /= 4 then
    Except.throwError FrameSizeError
  else do
    increment <- lift $ flip Bits.clearBit 31 <$> Get.getWord32be
    if increment == 0 then
      Except.throwError ProtocolError
    else
      return increment

putPayload :: Payload -> Put
putPayload = Put.putWord32be

toString :: String -> Payload -> String
toString prefix increment = prefix ++ show increment
