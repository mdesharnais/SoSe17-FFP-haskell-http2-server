{-# LANGUAGE OverloadedStrings #-}
module StreamsImpl () where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent.STM
import Control.Monad.RWS
import qualified Control.Monad.Except as Except

import Streams
import ProjectPrelude
import ErrorCodes
import ConnectionM
import qualified Logger as Log
import Settings
import LoggerImpl ()

instance StreamMonad (ConnectionM mode) where
      newStream sid@(StreamId sid') = do
             (StreamId maxSid) <- gets stMaxStreamId
             when (sid' <= maxSid || even sid') $ do
                                          Log.log Log.Crit "incorrect next session id"
                                          Except.throwError $ ConnError ConnectionError ProtocolError
             modify $ \s -> s { stMaxStreamId = sid }
             settingVar <- asks stSettings
             settings <- liftIO $ readTVarIO settingVar
             streams <- gets stStreams
             when (fromIntegral (getMaxConcurrentStreams LocalEndpoint settings) < Map.size streams) $ do
                        Log.log Log.Crit "to many concurrent streams"
                        Except.throwError $ ConnError (StreamError sid) ProtocolError
             let localInitWindow = getInitialWindowSize LocalEndpoint settings
                 remoteInitWindow = getInitialWindowSize RemoteEndpoint settings
             streamData <- liftIO $ initStreamData localInitWindow remoteInitWindow
             modify $ \s -> s { stStreams = Map.insert sid streamData (stStreams s) }
      streamAddHeaderFrag bs sid = do
            streamData <- getStreamData sid
            liftIO $ atomically $ modifyTVar (streamHeaderFragment streamData) $ \headers -> BS.append headers bs
      getHeaders sid = do
              streamData <- getStreamData sid
              liftIO $ atomically $ do
                let fragment = streamHeaderFragment streamData
                headers <- readTVar fragment
                writeTVar fragment BS.empty
                return headers
      getResvStrWindow sid = do
              streamData <- getStreamData sid
              liftIO $ readTVarIO (sendWindow streamData)
      getResvConnWindow = do
              connResvVar <- asks stConnResvWindow
              liftIO $ readTVarIO connResvVar
      getConnStrSendWindows sid = do
              streamData <- getStreamData sid
              connSendWinVar <- asks stConnSendWindow
              liftIO $ atomically $ do
                       strWin <- readTVar (resvWindow streamData)
                       connWin <- readTVar connSendWinVar
                       if strWin == 0 || connWin == 0
                            then retry
                            else return $ min strWin connWin
      fetchSubSendWindows sid dec = do
             streamData <- getStreamData sid
             let sendWinStrV = sendWindow streamData
             sendWinConnV <- asks stConnSendWindow
             liftIO $ atomically $ do
                 winS <- readTVar sendWinStrV
                 winC <- readTVar sendWinConnV
                 let strDec = min dec (cutToWord winS)
                     connDec = min dec (cutToWord winC)
                     dec'  = min strDec connDec
                 if dec' > 0
                     then do
                        writeTVar sendWinStrV (winS - (fromIntegral dec'))
                        writeTVar sendWinConnV (winC - (fromIntegral dec'))
                        return dec'
                     else retry
      addStrSendWindow sid update = do
                streamData <- getStreamData sid
                let sendWinV = sendWindow streamData
                success <- liftIO $ atomically $ do
                     oldVar <- readTVar sendWinV
                     let newVar = oldVar + (fromIntegral update)
                     writeTVar sendWinV (min newVar maximalWindowSize)
                     return $ newVar <= maximalWindowSize
                when (not success) $ do
                            Log.log Log.Crit "stream send window to big"
                            Except.throwError $ ConnError (StreamError sid) FlowControlError
      addConnSendWindow update = do
               sendWinV <- asks stConnSendWindow
               success <- liftIO $ atomically $ do
                   oldVar <- readTVar sendWinV
                   let newVar = oldVar + (fromIntegral update)
                   writeTVar sendWinV (min newVar maximalWindowSize)
                   return $ newVar <= maximalWindowSize
               when (not success) $ do
                          Log.log Log.Crit "connection send window to big"
                          Except.throwError $ ConnError ConnectionError FlowControlError
      fetchSubResvWindows sid dec = do
            streamData <- getStreamData sid
            let resvWinStrV = resvWindow streamData
            resvWinConnV <- asks stConnResvWindow
            liftIO $ atomically $ do
                  oldStrWin <- readTVar resvWinStrV
                  oldConnWin <- readTVar resvWinConnV
                  let newStrWin  = oldStrWin - (fromIntegral dec)
                      newConnWin = oldConnWin - (fromIntegral dec)
                  if newStrWin < 0 || newConnWin < 0
                        then return Nothing
                        else do
                           writeTVar resvWinStrV newStrWin
                           writeTVar resvWinConnV newConnWin
                           return $ Just (fromIntegral newStrWin, fromIntegral newConnWin)
      updateStrResvWindowTo sid newWin = do
             when (sid == (StreamId 0)) $ do
                               Log.log Log.Crit "must not be root stream"
                               Except.throwError $ ConnError ConnectionError InternalError
             streamData <- getStreamData sid
             let resvWin = resvWindow streamData
             liftIO $ atomically $ do
                    oldWin <- readTVar resvWin
                    let newWin' = fromIntegral newWin
                    if newWin' > oldWin
                           then do
                               writeTVar resvWin newWin'
                               return $ fromIntegral (newWin' - oldWin)
                           else return 0
      updateConnResvWindowTo newWin = do
             connResvWinVar <- asks stConnResvWindow
             liftIO $ atomically $ do
                   oldWin <- readTVar connResvWinVar
                   let newWin' = fromIntegral newWin
                   if newWin' > oldWin
                          then do
                             writeTVar connResvWinVar newWin'
                             return $ fromIntegral (newWin' - oldWin)
                          else return 0
      resvAction sid = do
              streamData <- getStreamData sid
              return $ resciveAction $ resvChan streamData
      execSendAction sendAc len = liftIO $ sendAc len
      getStreamState sid = do
              streamDataM <- gets $ (Map.lookup sid) . stStreams
              case streamDataM of
                     Nothing -> return StreamIdle
                     Just datas -> liftIO $ readTVarIO (streamState datas)
      setStreamState sid newState = do
              streamData <- getStreamData sid
              oldState <- liftIO $ readTVarIO (streamState streamData)
              if checkStateTrans oldState newState
                 then liftIO $ atomically $ writeTVar (streamState streamData) newState
                 else do
                    Log.log Log.Crit "illegale state transaction"
                    Except.throwError $ ConnError (StreamError sid) ProtocolError
      resvData sid bs = do
              streamData <- getStreamData sid
              liftIO $ atomically $ writeTChan (resvChan streamData) bs

resciveAction :: TChan ByteString -> (Int -> IO ByteString)
resciveAction chan count = do
                bs <- atomically $ readTChan chan
                if BS.null bs
                   then do
                      atomically $ unGetTChan chan BS.empty
                      return BS.empty
                   else if BS.length bs >= (fromIntegral count)
                     then return bs
                     else do
                        bs' <- BS.append bs <$> resvAc (fromIntegral count - BS.length bs)
                        let (result, rest) = BS.splitAt (fromIntegral count) bs'
                        when (not $ BS.null rest) $ atomically $ unGetTChan chan rest
                        return result
         where resvAc count' = do
                      mbs <- atomically $ tryReadTChan chan
                      case mbs of
                          Nothing -> return BS.empty
                          Just bs -> if BS.length bs >= count'
                                       then return bs
                                       else BS.append bs <$> resvAc (count' - BS.length bs)

getStreamData :: StreamId -> ConnectionM mode PerStreamData
getStreamData sid = do
        streamData <- gets $ (Map.lookup sid) . stStreams
        case streamData of
            Nothing -> do
                 Log.log Log.Crit "query non existent stream"
                 Except.throwError $ ConnError ConnectionError ProtocolError
            Just sData -> return sData

checkStateTrans :: StreamState -> StreamState -> Bool
checkStateTrans StreamIdle (StreamOpen {}) = True
checkStateTrans (StreamOpen {}) StreamClosed = True
checkStateTrans (StreamOpen { stHeaderEnd = False, stStreamEnd = end1 }) (StreamOpen { stHeaderEnd = True, stStreamEnd = end2 }) =
                                              end1 == end2
checkStateTrans _ (StreamRst _) = True
checkStateTrans s1 s2 = s1 == s2
