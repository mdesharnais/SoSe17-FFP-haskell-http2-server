module Frame.Continuation 
    ( Payload
    , endHeadersF
    , isEndHeaders
    , mkPayload
    , getHeaderFragment
    , getPayload
    , putPayload
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Control.Monad.Except (ExceptT, lift)
import qualified Data.Binary.Put as Put
import Data.Binary.Put (Put)

import ProjectPrelude
import ErrorCodes

data Payload = Payload ByteString

endHeadersF :: FrameFlags
endHeadersF = 0x4

isEndHeaders :: FrameFlags -> Bool
isEndHeaders f = testFlag f endHeadersF
  
getHeaderFragment :: Payload -> ByteString
getHeaderFragment (Payload bs) = bs

mkPayload :: ByteString -> Payload
mkPayload bs = Payload bs

getPayload :: FrameLength -> FrameFlags -> StreamId -> ExceptT ConnError Get Payload
getPayload fLength _ _ = do
            fmap Payload $ lift $ Get.getLazyByteString $ fromIntegral fLength 

putPayload :: Payload -> Put
putPayload (Payload bs) = Put.putLazyByteString bs
