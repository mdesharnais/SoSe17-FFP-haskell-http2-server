module ProjectPrelude where

import qualified Data.Word

type Word8  = Data.Word.Word8
type Word16 = Data.Word.Word16
type Word32 = Data.Word.Word32

type FrameLength = Word32
type FrameFlags = Word8
newtype StreamId = StreamId Word32 deriving Show

data ErrorCode =
  ProtocolError |
  FrameSizeError
