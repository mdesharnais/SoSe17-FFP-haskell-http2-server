module Frame.Internal.Padding where

import qualified Control.Monad.Except as Except
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.Bits as Bits

import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.ByteString.Lazy(ByteString)

import ProjectPrelude
import ErrorCodes

data PaddingDesc = PaddingDesc {
  ppLength :: Word8,
  ppPadding :: ByteString
} deriving Show

testPadFlag :: FrameFlags -> Bool
testPadFlag = flip Bits.testBit 3

getLength :: FrameLength -> FrameFlags -> ExceptT ConnError Get (FrameLength, Maybe Word8)
getLength len flags =
  if testPadFlag flags then
    if len >= 1 then
      lift $ (,) (len - 1) . Just <$> Get.getWord8
    else
      Except.throwError $ ConnError ConnectionError ProtocolError
  else 
    return (len, Nothing)

putLength :: Maybe PaddingDesc -> Put
putLength Nothing = return ()
putLength (Just PaddingDesc { ppLength }) = Put.putWord8 ppLength

getPadding :: Maybe Word8 -> Get (Maybe PaddingDesc)
getPadding Nothing = return Nothing
getPadding (Just ppLength) = do
  ppPadding <- Get.getLazyByteString (fromIntegral ppLength)
  return $ Just $ PaddingDesc { ppLength, ppPadding }

putPadding :: Maybe PaddingDesc -> Put
putPadding Nothing = return ()
putPadding (Just PaddingDesc { ppPadding }) = Put.putLazyByteString ppPadding
