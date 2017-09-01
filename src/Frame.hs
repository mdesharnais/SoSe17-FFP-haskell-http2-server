module Frame( Frame(..)
            , Type(..)
            , Payload(..)
            , get
            , toString
            , writeFrame 
            ) where

import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.Bits as Bits

import qualified Frame.Data as FData
import qualified Frame.Headers as FHeaders
import qualified Frame.Settings as FSettings
import qualified Frame.WindowUpdate as FWindowUpdate
import qualified Frame.Continuation as FContinuation

import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.Bits((.|.), (.&.))
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS (length)

import ProjectPrelude
import ErrorCodes

data Type = TData
  | THeaders
  | TPriority
  | TRstStream
  | TSettings
  | TPushPromise
  | TPing
  | TGoaway
  | TWindowUpdate
  | TContinuation
  | TUnknown
  deriving (Show, Eq)

data Payload = PData FData.Payload
  | PHeaders FHeaders.Payload
  | PSettings FSettings.Payload
  | PWindowUpdate FWindowUpdate.Payload
  | PContinuation FContinuation.Payload
  | PBuffer ByteString

data Frame = Frame {
  -- fType :: Type,          -- FIXME redundant info. Contained in Payload
  fFlags :: FrameFlags,
  fStreamId :: StreamId,
  fPayload :: Payload
}

typeOfPayload :: Payload -> Type
typeOfPayload (PData _) = TData
typeOfPayload (PHeaders _) = THeaders
typeOfPayload (PSettings _) = TSettings
typeOfPayload (PWindowUpdate _) = TWindowUpdate
typeOfPayload (PContinuation _) = TContinuation
typeOfPayload (PBuffer _) = TUnknown

getLength :: Get FrameLength
getLength = do
  b3 <- Get.getWord8
  b2 <- Get.getWord8
  b1 <- Get.getWord8
  let shift b = Bits.shift (fromIntegral b)
  return (shift b3 16 .|. shift b2 8 .|. fromIntegral b1)

putLength :: FrameLength -> Put
putLength len =
  let shift n = fromIntegral . (.&. 0xFF) . Bits.shiftR n in do
  Put.putWord8 (shift len 16)
  Put.putWord8 (shift len 8)
  Put.putWord8 (shift len 0)

getType :: Get Type
getType = do
  byte <- Get.getWord8
  return $ case byte of
    0x0 -> TData
    0x1 -> THeaders
    0x2 -> TPriority
    0x3 -> TRstStream
    0x4 -> TSettings
    0x5 -> TPushPromise
    0x6 -> TPing
    0x7 -> TGoaway
    0x8 -> TWindowUpdate
    0x9 -> TContinuation
    _   -> TUnknown

putType :: Type -> Put
putType TData         = Put.putWord8 0x0
putType THeaders      = Put.putWord8 0x1
putType TPriority     = Put.putWord8 0x2
putType TRstStream    = Put.putWord8 0x3
putType TSettings     = Put.putWord8 0x4
putType TPushPromise  = Put.putWord8 0x5
putType TPing         = Put.putWord8 0x6
putType TGoaway       = Put.putWord8 0x7
putType TWindowUpdate = Put.putWord8 0x8
putType TContinuation = Put.putWord8 0x9
putType TUnknown      = undefined

getStreamId :: Get StreamId
getStreamId = StreamId . flip Bits.clearBit 31 <$> Get.getWord32be

putStreamId :: StreamId -> Put
putStreamId (StreamId i) = Put.putWord32be (Bits.clearBit i 31)

getPayload :: FrameLength -> FrameFlags -> StreamId -> Type -> ExceptT ErrorCode Get Payload
getPayload len flags sId TData         = PData         <$> FData.getPayload         len flags sId
getPayload len flags sId THeaders      = PHeaders      <$> FHeaders.getPayload      len flags sId
getPayload len flags sId TSettings     = PSettings     <$> FSettings.getPayload     len flags sId
getPayload len flags sId TWindowUpdate = PWindowUpdate <$> FWindowUpdate.getPayload len flags sId
getPayload len flags sId TContinuation = PContinuation <$> FContinuation.getPayload len flags sId
getPayload len _     _   _         = lift $ PBuffer <$> Get.getLazyByteString (fromIntegral len)

putPayload :: Payload -> Put
putPayload (PData payload)         = FData.putPayload         payload
putPayload (PHeaders payload)      = FHeaders.putPayload      payload
putPayload (PSettings payload)     = FSettings.putPayload     payload
putPayload (PWindowUpdate payload) = FWindowUpdate.putPayload payload
putPayload (PContinuation payload) = FContinuation.putPayload payload
putPayload (PBuffer buffer)        = Put.putLazyByteString    buffer

get :: ExceptT ErrorCode Get Frame
get = do
  fLength <- lift $ getLength
  fType <- lift $ getType
  fFlags <- lift $ Get.getWord8
  fStreamId <- lift $ getStreamId
  fPayload <- getPayload fLength fFlags fStreamId fType
  return $ Frame { {-fLength, fType,-} fFlags, fStreamId, fPayload }

put :: Frame -> Put
put Frame { {-fLength,-} {-fType,-} fFlags, fStreamId, fPayload } = do
  let payload = Put.runPut $ putPayload fPayload
  putLength $ fromIntegral $ BS.length payload -- TODO exacte Laenge nachschauen
  putType $ typeOfPayload fPayload
  Put.putWord8 fFlags
  putStreamId fStreamId
  Put.putLazyByteString payload

writeFrame :: Frame -> ByteString
writeFrame = Put.runPut . put

toString :: Frame -> String
toString Frame { {-fType,-} fStreamId, fPayload } =
  let StreamId i = fStreamId in
  show (typeOfPayload fPayload) ++ "(" ++ show i ++ ")\n" ++
  case fPayload of
    PData _               -> "  Binary data"
    PHeaders payload      -> FHeaders.toString      "  " payload -- TODO
    PSettings payload     -> FSettings.toString     "  " payload
    PWindowUpdate payload -> FWindowUpdate.toString "  " payload
    _ -> ""
