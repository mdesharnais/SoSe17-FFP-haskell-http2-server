{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Handle.Headers
  ( handleHeaders
  , sendHeaders
  , handleContinuation
  ) where

import Control.Monad (when)
import qualified Control.Monad.State.Lazy as State
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Hpack
import Data.Text (Text, isPrefixOf)
import Data.List (unzip6)
import Data.Maybe (listToMaybe, catMaybes)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import ConnMonad
import ProjectPrelude
import Streams
import Handler
import Settings
import Frame (Frame (..))
import qualified Frame
import qualified Frame.Headers as FHeaders
import qualified Frame.Continuation as FContinuation
import ErrorCodes
import qualified Logger

handleHeaders :: (ConnMonad m) => FHeaders.Payload -> StreamId -> FrameFlags -> m ()
handleHeaders payload streamid flags = do
         when (streamid == StreamId 0) $ do
                          Logger.log Logger.Crit "headers frame on control stream"
                          throwError $ ConnError ConnectionError ProtocolError
         streamState <- getStreamState streamid
         when (streamState /= StreamIdle) $ do
                          Logger.log Logger.Crit "headers frame in this state not allowed"
                          throwError $ ConnError ConnectionError ProtocolError
         newStream streamid
         streamAddHeaderFrag (FHeaders.getHeaderFragment payload) streamid
         setStreamState streamid $ StreamOpen
                            { stStreamEnd = FHeaders.isEndStream flags
                            , stHeaderEnd = FHeaders.isEndHeaders flags
                            }
         if FHeaders.isEndHeaders flags
                 then handleHeaderComplete streamid flags
                 else do
                      setMoreHeaders (Just streamid)

sendHeaders :: (ConnMonad m) => Headers -> StreamId -> Bool -> m ()
sendHeaders headers sid hasData = do
             maxFrameSize <- getSetting $ getMaxFrameSize RemoteEndpoint
             let headerBuf = Put.runPut $ Hpack.putHeaderFields headers
                 headerFrags = splitChunks headerBuf $ fromIntegral maxFrameSize
             sendHeader headerFrags sid hasData
       where splitChunks bs len | BS.null bs = []
                                | BS.length bs <= len = [bs]
                                | otherwise = let (headBS, tailBS) = BS.splitAt len bs
                                                  in headBS : (splitChunks tailBS len)

sendHeader :: (ConnMonad m) => [ByteString] -> StreamId -> Bool -> m ()
sendHeader [] _ _ = throwError $ ConnError ConnectionError InternalError
sendHeader [frag] sid hasData = do
                   let fPayload = Frame.PHeaders $ FHeaders.mkPayload frag
                       fFlags = if hasData 
                                   then FHeaders.endHeadersF
                                   else setFlag FHeaders.endHeadersF FHeaders.endStreamF
                       frame = Frame { fPayload, fFlags, fStreamId = sid }
                   sendFrame frame
sendHeader (frag:frags) sid hasData = do
                   let fPayload = Frame.PHeaders $ FHeaders.mkPayload frag
                       fFlags = if hasData 
                                  then 0
                                  else FHeaders.endStreamF
                       frame = Frame { fPayload, fFlags, fStreamId = sid }
                   sendFrame frame
                   sendContinuation frags sid

sendContinuation :: (ConnMonad m) => [ByteString] -> StreamId -> m ()
sendContinuation [] _ = throwError $ ConnError ConnectionError InternalError
sendContinuation [frag] sid = do
                   let fPayload = Frame.PContinuation $ FContinuation.mkPayload frag
                       fFlags = FContinuation.endHeadersF
                       frame = Frame { fPayload, fFlags, fStreamId = sid }
                   sendFrame frame
sendContinuation (frag:frags) sid = do
                   let fPayload = Frame.PContinuation $ FContinuation.mkPayload frag
                       fFlags = 0
                       frame = Frame { fPayload, fFlags, fStreamId = sid }
                   sendFrame frame
                   sendContinuation frags sid

handleContinuation :: (ConnMonad m) => FContinuation.Payload -> StreamId -> FrameFlags -> m ()
handleContinuation payload streamid flags = do
         when (streamid == StreamId 0) $ do
                              Logger.log Logger.Crit "continuation frame on control stream"
                              throwError $ ConnError ConnectionError ProtocolError
         streamState <- getStreamState streamid
         case streamState of
               StreamOpen { stHeaderEnd = False } -> return ()
               _ -> do
                   Logger.log Logger.Crit "continuation frame in this state not allowed"
                   throwError $ ConnError ConnectionError ProtocolError
         streamAddHeaderFrag (FContinuation.getHeaderFragment payload) streamid
         setStreamState streamid $ streamState { stHeaderEnd = FContinuation.isEndHeaders flags }
         when (FContinuation.isEndHeaders flags) $ do
                    setMoreHeaders Nothing
                    handleHeaderComplete streamid flags

handleHeaderComplete :: (ConnMonad m) => StreamId -> FrameFlags -> m ()
handleHeaderComplete streamid _flags = do
        headerBuf <- getHeaders streamid
        let headers = Get.runGet (State.evalStateT Hpack.getHeaderFields []) headerBuf -- TODO Dynamic table speichern/laden 
                                                                                       -- hpack efficientere version
        (reqMethod, reqScheme, reqPath, reqAuthority, reqHeaders) <- processHeaders streamid headers
        streamState <- getStreamState streamid
        reqDataChunk <- case streamState of
                       StreamOpen { stStreamEnd = True } -> return Nothing
                       StreamOpen {} -> do
                                   resvAc <- resvAction streamid
                                   return $ Just resvAc
                       _ -> throwError $ ConnError ConnectionError InternalError
        let req = Request { reqMethod, reqScheme, reqPath, reqAuthority, reqHeaders, reqDataChunk }
        runHandler streamid req
   
processHeaders :: (ConnMonad m) => StreamId ->  Hpack.Headers -> m (HTTPMethod, Text, Text, Maybe Text, Hpack.Headers)
processHeaders sid headers = do
           let (methods, schemes, paths, authorities, others, vailds) = unzip6 $ processHeader <$> headers
           case (catMaybes methods, catMaybes schemes, catMaybes paths, catMaybes authorities, and vailds) of
                     ([method], [scheme], [path], authority, True) | length authority <= 1 ->
                                return (method, scheme, path, listToMaybe authority, catMaybes others)
                     _ -> do
                          Logger.log Logger.Crit "required pseudo header not given or given more than onces"
                          throwError $ ConnError (StreamError sid) ProtocolError
      
processHeader :: Hpack.HeaderField -> (Maybe HTTPMethod,Maybe Text,Maybe Text,Maybe Text,Maybe Hpack.HeaderField, Bool)
processHeader (":method", meth) = (methodFromText meth, Nothing, Nothing, Nothing, Nothing, True)
processHeader (":scheme", scheme) = (Nothing, Just scheme, Nothing, Nothing, Nothing, True)
processHeader (":path", path) = (Nothing, Nothing, Just path, Nothing, Nothing, True)
processHeader (":authority", auth) = (Nothing, Nothing, Nothing, Just auth, Nothing, True)
processHeader (head, value) | isPrefixOf ":" head = (Nothing, Nothing, Nothing, Nothing, Nothing, False)
                            | otherwise      = (Nothing, Nothing, Nothing, Nothing, Just (head, value), True)

methodFromText :: Text -> Maybe HTTPMethod
methodFromText "GET" = Just HTTP_GET
methodFromText "POST" = Just HTTP_POST
methodFromText "HEAD" = Just HTTP_HEAD
methodFromText "PUT" = Just HTTP_PUT
methodFromText "DELETE" = Just HTTP_DELETE 
methodFromText _ = Nothing

