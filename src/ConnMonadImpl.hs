{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ConnMonadImpl () where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Control.Monad.Except as Except
import Control.Monad.RWS
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Data.String (fromString)

import ConnMonad
import ConnectionM
import ErrorCodes
import ProjectPrelude
import Handler
import ServerConfig
import qualified Logger as Log
import qualified Handle.Headers as HHeaders
import qualified Handle.Data as HData
import LoggerImpl ()
import SettingsImpl ()
import StreamsImpl ()
import ServerConfig ()

instance (ConnMode mode) => ConnMonad (ConnectionM mode) where
       pushBackBuffer bs = do
            buffer <- gets stBuffer
            modify $ \s -> s { stBuffer = BS.append bs buffer }
       throwError err = Except.throwError err
       runHandler = runHandlerImpl
       moreHeadersExpected = gets stExpectMoreHeaders
       setMoreHeaders mh = modify $ \s -> s { stExpectMoreHeaders = mh }
       sendFrame frame = do
              sendChan <- asks stSendChan
              liftIO $ atomically $ writeTChan sendChan frame
       isConnEnd = do
            endVar <- asks stEndStream
            liftIO $ readTVarIO endVar
       setConnEnd = do
            endVar <- asks stEndStream
            liftIO $ atomically $ writeTVar endVar True
       getDynamicTable LocalEndpoint = gets stLocalDynTable
       getDynamicTable RemoteEndpoint = gets stRemoteDynTable
       setDynamicTable LocalEndpoint table = do
             modify $ \s -> s { stLocalDynTable = table }
       setDynamicTable RemoteEndpoint table = do
             modify $ \s -> s { stRemoteDynTable = table }

runHandlerImpl :: (ConnMode mode) => StreamId -> Request -> ConnectionM mode ()
runHandlerImpl sid req = do
        handler <- asks $ servHandler . stServerConfig
        config <- ask
        state <- get
        _ <- liftIO $ forkIO $ handler req >>= handleResponse sid state config
        return ()

writeResponse :: (ConnMode mode) => StreamId -> Response -> ConnectionM mode ()
writeResponse sid (Response { respStatus, respHeaders, respData }) = do
                       when (respStatus < 100 || respStatus >= 600) $ do
                                       Log.log Log.Crit "illegale HTTP status code"
                                       throwError $ ConnError ConnectionError InternalError
                       let headers = (":status", fromString $ show respStatus) : respHeaders
                           hasData = case respData of
                                      ResponseHeadOnly -> False
                                      _ -> True
                       HHeaders.sendHeaders headers sid hasData
                       HData.sendData respData sid

handleResponse :: (ConnMode mode) => StreamId -> ConnState -> ConnReader mode -> Response -> IO ()
handleResponse sid state reader resp = do
               res <- evalConnectionM (writeResponse sid resp) (ConnStateConfig state reader)
               case res of
                   Right _ -> return ()
                   Left _err -> undefined

instance forall mode. (ConnMode mode) => NetworkMonad (ConnectionM mode) where
       writeBuffer bs = do
            conn <- asks stSocket
            liftIO $ ((modeSendAll conn bs) :: IO (NMaybe mode))
            return ()
       readBuffer = do
            buf <- gets stBuffer
            conn <- asks stSocket
            if BS.null buf
                then do
                    (RRight buf2) <- liftIO $ ((modeRecv conn 1024) :: IO (REither mode ByteString))
                    if BS.null buf2
                          then do
                             setConnEnd
                             throwError $ ConnError ConnectionError EndOfConn
                          else return buf2
                else do
                    modify $ \s -> s { stBuffer = BS.empty }
                    return buf


