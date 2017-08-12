module Frame.Data(
  Payload,
  getPayload,
  mkPayload,
  putPayload
) where

import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Frame.Internal.Padding as Padding

import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.ByteString.Lazy(ByteString)
import Frame.Internal.Padding(PaddingDesc(..))

import ProjectPrelude

data Payload = Payload {
  pPadding :: Maybe PaddingDesc,
  pData    :: ByteString
}

getPayload :: FrameLength -> FrameFlags -> StreamId -> ExceptT ErrorCode Get Payload
getPayload fLength flags _ = do
  (fLength, paddingLength) <- Padding.getLength fLength flags
  pData <- lift $ Get.getLazyByteString $
    fromIntegral (maybe fLength ((fLength -) . fromIntegral) paddingLength)
  pPadding <- lift $ Padding.getPadding paddingLength
  return $ Payload { pPadding, pData }

putPayload :: Payload -> Put
putPayload Payload { pPadding, pData } = do
  Padding.putLength pPadding
  Put.putLazyByteString pData
  Padding.putPadding pPadding

mkPayload :: ByteString -> Payload
mkPayload pData = Payload { pPadding = Nothing, pData }
