{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Handle.Settings
   ( handleSettings
   , sendSettings
   , buildSettings
   , sendAcknowlege
   ) where

import Control.Monad (when)
import Data.Maybe (catMaybes)

import Frame (Frame (..))
import qualified Frame
import ConnMonad
import ProjectPrelude
import ErrorCodes
import qualified Frame.Settings as FSettings
import Settings
import qualified Logger

handleSettings :: (ConnMonad m) => FSettings.Payload -> StreamId -> FrameFlags -> m ()
handleSettings payload sid flags = do
   when (sid /= StreamId 0) $ do
                    Logger.log Logger.Crit "settings must be have stream id 0"
                    throwError $ ConnError ConnectionError ProtocolError
   if FSettings.isAcknowlegement flags
      then case payload of
              [] -> return ()
              _  -> do
                  Logger.log Logger.Crit "setting acknowlegements must have empty payload"
                  throwError $ ConnError ConnectionError ProtocolError
      else do
        handleParams payload
        sendAcknowlege

sendSettings :: (ConnMonad m) => [(FSettings.Setting, Word32)] -> m ()
sendSettings payload = sendFrame $ buildSettings payload

buildSettings :: [(FSettings.Setting, Word32)] -> Frame
buildSettings payload = let fPayload = Frame.PSettings payload
                            fFlags = emptyFlags
                            fStreamId = StreamId 0
                          in Frame { fPayload, fStreamId, fFlags }

sendAcknowlege :: (ConnMonad m) => m ()
sendAcknowlege = do
         let fFlags = FSettings.acknowlegementF
             fStreamId = StreamId 0
             fPayload = Frame.PSettings []
         sendFrame $ Frame { fPayload, fStreamId, fFlags }

handleParams :: (ConnMonad m) => [FSettings.Param] -> m ()
handleParams params = do
           case catMaybes $ checkParam <$> params of
                   (err : _) -> do
                             Logger.log Logger.Crit "invalid setting found"
                             throwError $ ConnError ConnectionError err
                   [] -> mapM_ applyParam params

checkParam :: FSettings.Param -> Maybe ErrorCode
checkParam (FSettings.EnablePush, i) | i /= 0 && i /= 1 = Just ProtocolError
checkParam (FSettings.InitialWindowSize, i) | i > 2^(31::Int) - 1 = Just FlowControlError
checkParam (FSettings.MaxFrameSize, i) | i < 2^(14::Int) || i > 2^(24::Int) - 1 = Just ProtocolError
checkParam _ = Nothing

applyParam :: (ConnMonad m) => FSettings.Param -> m ()
applyParam (FSettings.HeaderTableSize,i) = modifySettings $ setHeaderTableSize RemoteEndpoint i
applyParam (FSettings.EnablePush, i) = modifySettings $ setEnablePush RemoteEndpoint (i /= 0)
applyParam (FSettings.MaxConcurrentStreams, i) = modifySettings $ setMaxConcurrentStreams RemoteEndpoint i
applyParam (FSettings.InitialWindowSize, i) = do
                            currWin <- getSetting $ getInitialWindowSize RemoteEndpoint
                            adjustRemoteWindowSize (fromIntegral i - fromIntegral currWin) i
                            modifySettings $ setInitialWindowSize RemoteEndpoint i
applyParam (FSettings.MaxFrameSize, i) = modifySettings $ setMaxFrameSize RemoteEndpoint i
applyParam (FSettings.MaxHeaderListSize, i) = modifySettings $ setMaxHeaderListSize RemoteEndpoint i
applyParam (FSettings.Unknown _,_) = modifySettings $ id
