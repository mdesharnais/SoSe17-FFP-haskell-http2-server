module ProjectPrelude
  ( FrameFlags
  , FrameLength
  , StreamId (..)
  , Endpoint (..)
  , oo
  , ooo
  , oooo
  , testFlag
  , setFlag
  , emptyFlags
  , Word8
  , Word16
  , Word32
  , Int64
  , cutToWord
  ) where

import Data.Word (Word8, Word16, Word32)
import Data.Bits
import Data.Int (Int64)

type FrameLength = Word32
type FrameFlags = Word8
newtype StreamId = StreamId Word32 deriving (Show, Eq, Ord)


data Endpoint = LocalEndpoint | RemoteEndpoint deriving (Show, Eq)

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

cutToWord :: Int64 -> Word32
cutToWord win = if win < 0
                      then 0
                      else fromIntegral win
