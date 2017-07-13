module Frame where

import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Data.Maybe

data FrameType = DATAFrameT

frameTypeToBinary :: FrameType -> Word8
frameTypeToBinary DATAFrameT = 0x0
frameTypeToBinary _ = undefined

frameTypeFromBinary :: Word8 -> Maybe FrameType
frameTypeFromBinary 0x0 = Just DATAFrameT
frameTypeFromBinary _= undefined

class H2Frame f where
       frameLen :: f -> Word32
       frameType :: f -> FrameType
       frameFlags :: f -> Word8
       frameStrId :: f -> Word32

data DATAFrame = DATAFrame { dataPadLen :: Maybe Word8
                           , dataEndStr :: Bool
                           , dataContent :: B.ByteString
                           , dataStrId :: Word32
                           }

instance H2Frame DATAFrame where
      frameLen dframe = let contLen = B.length $ dataContent dframe
                            padlen = case dataPadLen dframe of
                                        Nothing -> 0
                                        Just pad -> 1 + fromIntegral pad
                         in fromIntegral $ contLen + padlen
      frameType _ = DATAFrameT
      frameFlags dframe = let end = if dataEndStr dframe then 1 else 0
                              pad = if isJust $ dataPadLen dframe then 1 else 0
                           in end .|. pad
      frameStrId dframe = 0x7fff .&. frameStrId dframe
