{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Handle.Headers where

import Control.Monad (when)
import qualified Control.Monad.State.Lazy as State
import qualified Data.Binary.Get as Get
import qualified Hpack
import Data.Text (Text, isPrefixOf)
import Data.List (unzip6)
import Data.Maybe (listToMaybe, catMaybes)

import ConnectionMonad 
import ProjectPrelude
import Streams
import Handler
import qualified Frame.Headers as FHeaders
import qualified Frame.Continuation as FContinuation

handleHeaders :: (ConnMonad m) => FHeaders.Payload -> StreamId -> FrameFlags -> m Bool -- TODO kein Bool, wenn Fehler dann Error
handleHeaders payload streamid flags = do
         when (streamid == StreamId 0) $ throwError ProtocolError
         streamState <- getStreamState streamid
         when (streamState /= StreamIdle) $ throwError ProtocolError
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
                      return True

handleContinuation :: (ConnMonad m) => FContinuation.Payload -> StreamId -> FrameFlags -> m Bool
handleContinuation payload streamid flags = do
         when (streamid == StreamId 0) $ throwError ProtocolError
         streamState <- getStreamState streamid
         case streamState of
               StreamOpen { stHeaderEnd = False } -> return ()
               _ -> throwError ProtocolError
         streamAddHeaderFrag (FContinuation.getHeaderFragment payload) streamid
         setStreamState streamid $ streamState { stHeaderEnd = FContinuation.isEndHeaders flags }
         if FContinuation.isEndHeaders flags 
                 then do 
                    setMoreHeaders Nothing
                    handleHeaderComplete streamid flags
                 else return True

handleHeaderComplete :: (ConnMonad m) => StreamId -> FrameFlags -> m Bool
handleHeaderComplete streamid _flags = do
        headerBuf <- getHeaders streamid
        let headers = Get.runGet (State.evalStateT Hpack.getHeaderFields []) headerBuf -- TODO Dynamic table speichern/laden 
                                                                                       -- hpack efficientere version
        (reqMethod, reqScheme, reqPath, reqAuthority, reqHeaders) <- processHeaders headers
        streamState <- getStreamState streamid
        reqDataChunk <- case streamState of
                       StreamOpen { stStreamEnd = True } -> return Nothing
                       StreamOpen {} -> do
                                   resvAc <- resvAction streamid
                                   return $ Just resvAc
                       _ -> throwError undefined -- Internal Error
        let req = Request { reqMethod, reqScheme, reqPath, reqAuthority, reqHeaders, reqDataChunk }
        runHandler req
        return True
   
data PseudoHeader = Pmethod Text | Pscheme Text | Ppath Text | Pauthority Text

processHeaders :: (ConnMonad m) => Hpack.Headers -> m (HTTPMethod, Text, Text, Maybe Text, Hpack.Headers)
processHeaders headers = do
           let (methods, schemes, paths, authorities, others, vailds) = unzip6 $ processHeader <$> headers
           case (catMaybes methods, catMaybes schemes, catMaybes paths, catMaybes authorities, and vailds) of
                     ([method], [scheme], [path], authority, True) | length authority <= 1 ->
                                return (method, scheme, path, listToMaybe authority, catMaybes others)
                     _ -> throwError ProtocolError -- stream error
      
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

