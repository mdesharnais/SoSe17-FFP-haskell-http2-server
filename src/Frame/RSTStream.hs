module Frame.RSTStream 
   ( Payload
   , putPayload
   , getPayload
   , toString
   ) where

import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Trans (lift)

import ProjectPrelude
import ErrorCodes

type Payload = ErrorCode

getPayload :: FrameLength -> FrameFlags -> StreamId -> ExceptT ErrorCode Get Payload
getPayload fLength _ _ =
  if fLength /= 4 then
    Except.throwError FrameSizeError -- connection error
  else lift $ errorCodeFromWord32 <$> Get.getWord32be

putPayload :: Payload -> Put
putPayload = Put.putWord32be . errorCodeToWord32

toString :: String -> Payload -> String
toString prefix errorCode = prefix ++ show errorCode
