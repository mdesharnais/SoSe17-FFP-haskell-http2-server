{-# LANGUAGE NamedFieldPuns #-}

module Frame(
  Frame,
  Type,
  fType,
  get,
  toString
) where

import qualified Data.Binary.Get as Get
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS

import qualified Frame.Settings as FSettings

import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Bits((.|.))
import Data.ByteString(ByteString)
import Data.Word(Word8, Word32)

import ProjectPrelude

data Type =
  TData |
  THeaders |
  TPriority |
  TRstStream |
  TSettings |
  TPushPromise |
  TPing |
  TGoaway |
  TWindowUpdate |
  TContinuation
  deriving Show

data Payload =
  PSettings FSettings.Payload |
  PBuffer ByteString

data Frame = Frame {
  fLength :: FrameLength,
  fType :: Type,
  fFlags :: FrameFlags,
  fStreamId :: StreamId,
  fPayload :: Payload
}

getLength :: Get FrameLength
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

getStreamId :: Get StreamId
getStreamId = do
  word <- Get.getWord32be
  return (StreamId (Bits.clearBit word 31))

getPayload :: FrameLength -> FrameFlags -> StreamId -> Type -> ExceptT ErrorCode Get Payload
getPayload length flags sId TSettings = PSettings <$> FSettings.getPayload length flags sId
getPayload length _     _   _         = lift $ PBuffer <$> Get.getByteString (fromIntegral length)

get :: ExceptT ErrorCode Get Frame
get = do
  fLength <- lift $ getLength
  fType <- lift $ getType
  fFlags <- lift $ Get.getWord8
  fStreamId <- lift $ getStreamId
  fPayload <- getPayload fLength fFlags fStreamId fType
  return $ Frame { fLength, fType, fFlags, fStreamId, fPayload }

toString :: Frame -> String
toString Frame { fLength, fType, fFlags, fStreamId, fPayload } =
  let StreamId id = fStreamId in
  show fType ++ "(" ++ show id ++ ")\n" ++
  case fPayload of
    PSettings payload -> FSettings.toString "  " payload
    _ -> ""
