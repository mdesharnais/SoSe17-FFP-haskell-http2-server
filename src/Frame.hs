{-# LANGUAGE NamedFieldPuns #-}

module Frame(
  Frame,
  Type,
  fType,
  get
) where

import qualified Data.Binary.Get as Get
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS

import Data.Binary.Get(Get)
import Data.Bits((.|.))
import Data.ByteString(ByteString)
import Data.Word(Word8, Word32)

data Type =
  Data |
  Headers |
  Priority |
  RstStream |
  Settings |
  PushPromise |
  Ping |
  Goaway |
  WindowUpdate |
  Continuation
  deriving Show

type Payload = ByteString

data Frame = Frame {
  fLength :: Word32,
  fType :: Type,
  fFlags :: Word8,
  fStreamId :: Word32,
  fPayload :: Payload
}

getLength :: Get Word32
getLength = do
  b3 <- Get.getWord8
  b2 <- Get.getWord8
  b1 <- Get.getWord8
  let shift b = Bits.shift (fromIntegral b)
  return (shift b3 16 .|. shift b2 8 .|. fromIntegral b1)

getType :: Get Type
getType = do
  byte <- Get.getWord8
  return $ case byte of
    0x0 -> Data
    0x1 -> Headers
    0x2 -> Priority
    0x3 -> RstStream
    0x4 -> Settings
    0x5 -> PushPromise
    0x6 -> Ping
    0x7 -> Goaway
    0x8 -> WindowUpdate
    0x9 -> Continuation

getStreamId :: Get Word32
getStreamId = do
  word <- Get.getWord32be
  return (Bits.clearBit word 31)

getPayload :: Word32 -> Type -> Get Payload
getPayload length _ = Get.getByteString (fromIntegral length)

get :: Get Frame
get = do
  fLength <- getLength
  fType <- getType
  fFlags <- Get.getWord8
  fStreamId <- getStreamId
  fPayload <- getPayload fLength fType
  return $ Frame { fLength, fType, fFlags, fStreamId, fPayload }
