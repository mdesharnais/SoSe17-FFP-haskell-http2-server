{-# LANGUAGE FlexibleContexts, NamedFieldPuns, OverloadedStrings #-}

module Connection(
  handleConnection
) where

-- import Control.Monad (when)
import qualified Control.Monad.Except as Except
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS

import Frame
import qualified Frame.Settings as FSettings

import qualified Handle.Headers as HHeaders
import qualified Handle.Settings as HSettings
import qualified Handle.Data as HData
import qualified Handle.WindowUpdate as HWindowsUpdate

import Network.Socket(Socket)

import Frame(Frame(..))
import ErrorCodes
import ConnMonad
import ConnectionM
import ConnMonadImpl ()
import qualified Logger
import ServerConfig

connPreface :: FSettings.Payload
connPreface = [(FSettings.EnablePush, 0)]

handleFrame :: (ConnMonad m) => Frame -> m ()
handleFrame (Frame {fPayload, fStreamId, fFlags}) = do
 expHeaders <- moreHeadersExpected 
 case (expHeaders, fPayload)  of
      (Nothing, _) -> return ()
      (Just streamid, PContinuation _ ) | streamid == fStreamId -> return ()
      _ -> do 
          Logger.log Logger.Crit "Continuation expected"
          throwError ProtocolError -- TODO Aendern entsprechend position von catchError
 case fPayload of
   (PSettings payload) -> HSettings.handleSettings payload fStreamId fFlags -- TODO
   (PHeaders payload) -> HHeaders.handleHeaders payload fStreamId fFlags
   (PContinuation payload) -> HHeaders.handleContinuation payload fStreamId fFlags
   (PData payload) -> HData.handleData payload fStreamId fFlags
   (PWindowUpdate payload) -> HWindowsUpdate.handleWindowUpdate payload fStreamId fFlags
   _ -> return ()

readFrame :: (ConnMonad m) => m Frame
readFrame = do
  let impl (Get.Fail _ _ _)          = do
                                Logger.log Logger.Crit "read frame failed"
                                throwError ProtocolError
      impl (Get.Partial continue)    = readBuffer >>= impl . continue . Just . ByteString.toStrict
      impl (Get.Done _ _ (Left err)) = do 
                                Logger.log Logger.Crit "error while reading frame"
                                throwError err
      impl (Get.Done buffer _ (Right frame)) = do
        pushBackBuffer $ ByteString.fromStrict buffer
        return frame
  frame <- impl (Get.runGetIncremental (Except.runExceptT Frame.get))
  Logger.log Logger.Info $ Text.pack (Frame.toString frame)
  return frame

handleConnection :: Socket -> ServerConfig -> IO ()
handleConnection sock config = do
  stateConfig <- initConnStateConfig sock config
  _ <- forkIO $ evalConnectionM (sendThread connPreface) stateConfig >> return ()
  result <- evalConnectionM resvThread stateConfig
  case result of
     Left _ -> undefined -- TODO
     Right _ -> return ()

runReader :: (ConnMonad m) => m () 
runReader = do
         end <- isStreamEnd
         when (not end) 
            $ readFrame >>= handleFrame >> runReader

resvThread :: ConnectionM ()
resvThread = Except.catchError resvThreadConn $ \s -> liftIO $ print s -- TODO catch connection errors

resvThreadConn :: (ConnMonad m) => m ()
resvThreadConn = do
    preface <- readFrame
    case fPayload preface of
         PSettings _ -> handleFrame preface >> return ()
         _ -> do
              Logger.log Logger.Crit "incorrect connection preface"
              throwError ProtocolError -- ConnectionError
    runReader
           
sendThread :: FSettings.Payload -> ConnectionM ()
sendThread preface = do
    let prefaceFrame = HSettings.buildSettings preface
    netSendFrame prefaceFrame
    sendChan <- asks stSendChan
    streamEnd <- asks stEndStream
    runSender sendChan streamEnd
  where runSender sendChan streamEnd = do
          frameM <- liftIO $ atomically $ readChannel sendChan streamEnd
          case frameM of
               Just frame -> do
                   netSendFrame frame 
                   runSender sendChan streamEnd
               Nothing -> return ()
        readChannel sendChan streamEnd = do
            frame <- tryReadTChan sendChan
            end <- readTVar streamEnd
            case (frame, end) of
                 (_  ,True) -> return Nothing
                 (Just _ , False) -> return frame
                 (Nothing, False) -> retry
        netSendFrame frame = writeBuffer $ Frame.writeFrame frame
