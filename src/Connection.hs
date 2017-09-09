{-# LANGUAGE FlexibleContexts, NamedFieldPuns, OverloadedStrings #-}

module Connection(
  handleConnection
) where

import qualified Control.Monad.Except as Except
import qualified Data.Binary.Get as Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS
import Data.String (fromString)

import Frame
import qualified Frame.Settings as FSettings

import qualified Handle.Headers as HHeaders
import qualified Handle.Settings as HSettings
import qualified Handle.Data as HData
import qualified Handle.WindowUpdate as HWindowsUpdate
import qualified Handle.Goaway as HGoaway
import qualified Handle.RSTStream as HRSTStream

import Frame(Frame(..))
import ErrorCodes
import ConnMonad
import ConnectionM
import ConnMonadImpl ()
import qualified Logger
import ServerConfig
import Streams
import ProjectPrelude

connPreface :: FSettings.Payload
connPreface = [(FSettings.EnablePush, 0)]

handleFrame :: (ConnMonad m) => Frame -> m ()
handleFrame (Frame {fPayload, fStreamId, fFlags}) = do
 streamState <- getStreamState fStreamId
 connEnd <- isConnEnd
 if streamState == StreamRst LocalEndpoint || connEnd
    then return ()
    else do
      expHeaders <- moreHeadersExpected
      case (expHeaders, fPayload)  of
           (Nothing, _) -> return ()
           (Just streamid, PContinuation _ ) | streamid == fStreamId -> return ()
           _ -> do
               Logger.log Logger.Crit "Continuation expected"
               throwError $ ConnError ConnectionError ProtocolError
      case fPayload of
        (PSettings payload) -> HSettings.handleSettings payload fStreamId fFlags
        (PHeaders payload) -> HHeaders.handleHeaders payload fStreamId fFlags
        (PContinuation payload) -> HHeaders.handleContinuation payload fStreamId fFlags
        (PData payload) -> HData.handleData payload fStreamId fFlags
        (PWindowUpdate payload) -> HWindowsUpdate.handleWindowUpdate payload fStreamId fFlags
        _ -> return ()

readFrame :: (ConnMonad m) => m Frame
readFrame = do
  let impl (Get.Fail _ _ _)          = do
                                Logger.log Logger.Crit "read frame failed"
                                throwError $ ConnError ConnectionError ProtocolError
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

handleConnection :: (ConnMode mode) => ConnModeSocket mode -> ServerConfig mode -> IO ()
handleConnection sock config = do
  stateConfig <- initConnStateConfig sock config
  _ <- forkIO $ evalConnectionM (sendThread connPreface) stateConfig >> return ()
  _ <- evalConnectionM resvThread stateConfig
  return ()

runReader :: (ConnMonad m) => m ()
runReader = do
         end <- isConnEnd
         when (not end)
            $ readFrame >>= handleFrame >> runReader

h2ConnectionPrefix :: ByteString
h2ConnectionPrefix = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

resvThread :: (ConnMode mode) => ConnectionM mode ()
resvThread = do
      let reqLen = fromIntegral (ByteString.length h2ConnectionPrefix)
      (msg, rest) <- getBuffer ByteString.empty reqLen
      when (not $ ByteString.null rest) $ pushBackBuffer rest
      if msg == h2ConnectionPrefix
            then do
               Logger.log Logger.Info  "HTTP/2 Connection prefix received."
               resvThreadRun
            else do
               Logger.log Logger.Crit "Invalid HTTP/2 connection prefix: "
               Logger.log Logger.Crit $ fromString (show msg)
   where getBuffer bs len =
           if ByteString.length bs < len
              then do
                buf <- readBuffer
                getBuffer (ByteString.append bs buf) len
              else return $ ByteString.splitAt len bs



resvThreadRun :: (ConnMode mode) => ConnectionM mode ()
resvThreadRun = do
         run <- Except.catchError (resvThreadConn >> return False) resvError
         if run
           then resvThread
           else return ()


resvError :: (ConnMode mode) => ConnError -> ConnectionM mode Bool
resvError (ConnError ConnectionError EndOfConn) = return False
resvError (ConnError ConnectionError errType) = do
               HGoaway.sendGoaway errType ByteString.empty
               setConnEnd
               return False
resvError (ConnError (StreamError sid) errType) = do
               setStreamState sid (StreamRst LocalEndpoint)
               HRSTStream.sendRstStream sid errType
               return True

resvThreadConn :: (ConnMonad m) => m ()
resvThreadConn = do
    preface <- readFrame
    case fPayload preface of
         PSettings _ -> handleFrame preface >> return ()
         _ -> do
              Logger.log Logger.Crit "incorrect connection preface"
              throwError $ ConnError ConnectionError ProtocolError
    runReader

sendThread :: (ConnMode mode) => FSettings.Payload -> ConnectionM mode ()
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
                 (Just _ , _) -> return frame
                 (Nothing, True) -> return Nothing
                 (Nothing, False) -> retry
        netSendFrame frame = writeBuffer $ Frame.writeFrame frame
