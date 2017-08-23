module ProjectPrelude where

import qualified Data.Word
import Data.Bits

type Word8  = Data.Word.Word8
type Word16 = Data.Word.Word16
type Word32 = Data.Word.Word32

type FrameLength = Word32
type FrameFlags = Word8
newtype StreamId = StreamId Word32 deriving (Show, Eq, Ord)

data ErrorCode = ProtocolError 
               | FrameSizeError 
               | FlowControlError

data Endpoint = LocalEndpoint | RemoteEndpoint deriving Show

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo = (.) . (.)

ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo = oo . (.)

oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo = ooo . (.)

testFlag :: FrameFlags -> FrameFlags -> Bool
testFlag flag reg = (flag .&. reg) /= 0

setFlag :: FrameFlags -> FrameFlags -> FrameFlags
setFlag flag reg = flag .|. reg

emptyFlags :: FrameFlags
emptyFlags = 0
