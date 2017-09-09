{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Handle.Data
   ( handleData
   , sendData
   ) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as BS
import Control.Monad (when)

import Frame (Frame (..))
import qualified Frame
import Handler
import ConnMonad
import qualified Frame.Data as FData
import qualified Handle.WindowUpdate as HWindowUpdate
import ProjectPrelude
import ErrorCodes
import Settings
import Streams
import qualified Logger

handleData :: (ConnMonad m) => FData.Payload -> StreamId -> FrameFlags -> m ()
handleData payload sid flags = do
        when (sid == (StreamId 0)) $ do
                          Logger.log Logger.Crit "data frame on control stream"
                          throwError $ ConnError ConnectionError ProtocolError
        streamState <- getStreamState sid
        when (streamState /= StreamOpen { stHeaderEnd = True, stStreamEnd = False }) $ do
                 Logger.log Logger.Crit "data frame are not allowed in this state"
                 throwError $ ConnError (StreamError sid) StreamClosedError
        when (testFlag FData.endStreamF flags) $ setStreamState sid StreamClosed
        let bs = FData.getData payload
        resvWinM <- fetchSubResvWindows sid (fromIntegral $ BS.length bs)
        case resvWinM of
              Nothing -> do
                      Logger.log Logger.Crit "violation of rescive window"
                      throwError $ ConnError (StreamError sid) ProtocolError
              Just _ -> do
                  resvData sid bs
                  HWindowUpdate.sendWindowUpdates sid
        when (testFlag FData.endStreamF flags) $ resvData sid BS.empty -- end of stream

sendData :: (ConnMonad m) => ResponseData -> StreamId -> m ()
sendData ResponseHeadOnly _ = return ()
sendData (ResponseComplete bs) sid = sendDataChunk bs sid
sendData (ResponseChunked sendAc) sid = sendDataChunked sendAc sid BS.empty

sendDataChunk :: (ConnMonad m) => ByteString -> StreamId -> m ()
sendDataChunk bs sid = do
       maxFrameSize <- getSetting $ getMaxFrameSize RemoteEndpoint
       frameSize <- fetchSubSendWindows sid maxFrameSize
       let (headBS, tailBS) = BS.splitAt (fromIntegral frameSize) bs
           endStream = BS.null tailBS
       sendByteString headBS sid endStream
       when (not endStream) $ sendDataChunk tailBS sid

sendDataChunked :: (ConnMonad m) => (Word32 -> IO ByteString) -> StreamId -> ByteString -> m ()
sendDataChunked sendAc sid buffer = do
       maxFrameSize <- getSetting $ getMaxFrameSize RemoteEndpoint
       frameSize <- fetchSubSendWindows sid maxFrameSize
       if BS.length buffer >= (fromIntegral frameSize)
             then do
                let (headBS, tailBS) = BS.splitAt (fromIntegral frameSize) buffer
                sendByteString headBS sid False
                sendDataChunked sendAc sid tailBS
             else do
                bs <- execSendAction sendAc $ frameSize - fromIntegral (BS.length buffer)
                let (headBS, tailBS) = BS.splitAt (fromIntegral frameSize) $ BS.append buffer bs
                    endStream = BS.null bs
                sendByteString headBS sid endStream
                when (not endStream) $ sendDataChunked sendAc sid tailBS

sendByteString :: (ConnMonad m) => ByteString -> StreamId -> Bool -> m ()
sendByteString bs fStreamId endStream = do
                   let fFlags = if endStream then FData.endStreamF else 0
                       fPayload = Frame.PData $ FData.mkPayload bs
                   sendFrame $ Frame { fFlags, fStreamId, fPayload }
