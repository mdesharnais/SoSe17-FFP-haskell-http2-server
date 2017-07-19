module Frame.Headers(
  Payload,
  getPayload,
  putPayload,
  toString
)where

import qualified Data.Binary.Get as Get
import qualified Data.Bits as Bits

import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.ByteString.Lazy(ByteString)
import Data.Word(Word8)

import ProjectPrelude

data PriorityDesc = PriorityDesc {
  pdExclusive :: Bool,
  pdDependency :: StreamId,
  pdWeight :: Word8
} deriving Show

data PaddingDesc = PaddingDesc {
  ppLength :: Word8,
  ppPadding :: ByteString
} deriving Show

data Payload = Payload {
  pPriority :: Maybe PriorityDesc,
  pPadding  :: Maybe PaddingDesc,
  pBlockFragment :: ByteString
}

testFlagPadded :: FrameFlags -> Bool
testFlagPadded = flip Bits.testBit 3

testFlagPriority :: FrameFlags -> Bool
testFlagPriority = flip Bits.testBit 5

getPriority :: Get PriorityDesc
getPriority = do
  w <- Get.getWord32be
  let pdExclusive = Bits.testBit w 31
  let pdDependency = StreamId (Bits.clearBit w 31)
  pdWeight <- Get.getWord8
  return $ PriorityDesc { pdExclusive, pdDependency, pdWeight }

getPayload :: FrameLength -> FrameFlags -> StreamId -> ExceptT ErrorCode Get Payload
getPayload fLength flags _ = do
  (fLength, paddingLength) <- lift $
    if testFlagPadded flags then 
      (,) (fLength - 1) . Just <$> Get.getWord8
    else 
      return (fLength, Nothing)
  (fLength, pPriority) <- lift $
    if testFlagPriority flags then
      (,) (fLength - 5) . Just <$> getPriority
    else
      return (fLength, Nothing)
  pBlockFragment <- lift $ Get.getLazyByteString $
    fromIntegral (maybe fLength ((fLength -) . fromIntegral) paddingLength)
  pPadding <- case paddingLength of
    Nothing -> return Nothing
    Just ppLength -> do
      ppPadding <- lift $ Get.getLazyByteString (fromIntegral ppLength)
      return $ Just $ PaddingDesc { ppLength, ppPadding }
  return $ Payload { pPriority, pPadding, pBlockFragment }

putPayload :: Payload -> Put
putPayload = undefined

toString :: String -> Payload -> String
toString prefix Payload { pPriority, pPadding } = unlines $ map (prefix++) [
    "priority: " ++ show pPriority,
    "padding: " ++ show pPadding
  ]
