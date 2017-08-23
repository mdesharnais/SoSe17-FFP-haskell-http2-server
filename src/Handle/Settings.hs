module Handle.Settings where

--import Data.Set(Set)
--import qualified Data.Set as Set
--import Control.Monad (when)
import Data.Maybe (catMaybes)

import Frame
import ConnectionMonad
import ProjectPrelude
import qualified Frame.Settings as FSettings
import Settings

handleSettings :: (ConnMonad m) => Payload -> StreamId -> FrameFlags -> m Bool
handleSettings = undefined
{-
handleSettings :: (ConnMonad m) => Frame -> m Bool
handleSettings frame@Frame { fType = Frame.TSettings, fFlags = 0x0 } = do
  when (fStreamId frame /= StreamId 0) $ throwError ProtocolError
  -- let (PSettings settings) = fPayload frame
  -- TODO handle settings
  writeBuffer $ Frame.writeFrame $ Frame {
    fType = Frame.TSettings,
    fFlags = 0x1,
    fStreamId = StreamId 0,
    fPayload = Frame.PSettings Set.empty
  }
  return True
handleSettings Frame { fType = Frame.TSettings, fFlags = 0x1 } = return True
handleSettings _ = undefined
-}

handleParams :: (ConnMonad m) => [FSettings.Param] -> m ()
handleParams params = do
           case catMaybes $ checkParam <$> params of
                   (err : _) -> throwError err
                   [] -> do modifySettings (compose $ applyParam <$> params) 
         where compose [] a = a
               compose (f:fs) a = compose fs (f a)

checkParam :: FSettings.Param -> Maybe ErrorCode
checkParam (FSettings.EnablePush, i) | i /= 0 && i /= 1 = Just ProtocolError
checkParam (FSettings.InitialWindowSize, i) | i > 2^(31::Int) - 1 = Just FlowControlError
checkParam (FSettings.MaxFrameSize, i) | i < 2^(14::Int) || i > 2^(24::Int) - 1 = Just ProtocolError
checkParam _ = Nothing

applyParam :: FSettings.Param -> ConnSettings -> ConnSettings
applyParam (FSettings.HeaderTableSize,i) = setHeaderTableSize RemoteEndpoint i
applyParam (FSettings.EnablePush, i) = setEnablePush RemoteEndpoint (i /= 0)
applyParam (FSettings.MaxConcurrentStreams, i) = setMaxConcurrentStreams RemoteEndpoint i
applyParam (FSettings.InitialWindowSize, i) = setInitialWindowSize RemoteEndpoint i
applyParam (FSettings.MaxFrameSize, i) = setMaxFrameSize RemoteEndpoint i
applyParam (FSettings.MaxHeaderListSize, i) = setMaxHeaderListSize RemoteEndpoint i
applyParam (FSettings.Unknown _,_) = id
