module ProjectPrelude where

import Data.Word(Word8, Word32)

type FrameLength = Word32
type FrameFlags = Word8
newtype StreamId = StreamId Word32

data ErrorCode =
  ProtocolError |
  FrameSizeError
