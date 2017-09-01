{-# LANGUAGE OverloadedStrings #-}
module ConnMonadImpl () where

import qualified Data.ByteString.Lazy as BS
import qualified Control.Monad.Except as Except
import Control.Monad.RWS
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Data.String (fromString)
import Network.Socket.ByteString as SocketBS

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

instance ConnMonad ConnectionM where
       pushBackBuffer bs = do
            buffer <- gets stBuffer
            modify $ \s -> s { stBuffer = BS.append bs buffer }
       throwError err = Except.throwError err
       runHandler = runHandlerImpl
       -- moreHeadersExpected :: m (Maybe StreamId)
       moreHeadersExpected = gets stExpectMoreHeaders
       -- setMoreHeaders :: (Maybe StreamId) -> m ()
       setMoreHeaders mh = modify $ \s -> s { stExpectMoreHeaders = mh }
       -- sendFrame :: Frame -> m ()
       sendFrame frame = do
              sendChan <- asks stSendChan
              liftIO $ atomically $ writeTChan sendChan frame
       -- isStreamEnd :: m Bool
       isStreamEnd = do
            endVar <- asks stEndStream
            liftIO $ readTVarIO endVar
       -- setStreamEnd :: m ()
       setStreamEnd = do
            endVar <- asks stEndStream
            liftIO $ atomically $ writeTVar endVar True

runHandlerImpl :: StreamId -> Request -> ConnectionM ()
runHandlerImpl sid req = do
        handler <- asks $ servHandler . stServerConfig
        config <- ask
        state <- get
        _ <- liftIO $ forkIO $ handler req >>= handleResponse sid state config
        return ()

writeResponse :: StreamId -> Response -> ConnectionM ()
writeResponse sid (Response { respStatus, respHeaders, respData }) = do
                       when (respStatus < 100 || respStatus >= 600) $ do
                                       Log.log Log.Crit "illegale HTTP status code"
                                       throwError undefined -- TODO internal error
                       let headers = (":status", fromString $ show respStatus) : respHeaders
                           hasData = case respData of
                                      ResponseHeadOnly -> False
                                      _ -> True
                       HHeaders.sendHeaders headers sid hasData
                       HData.sendData respData sid

handleResponse :: StreamId -> ConnState -> ConnReader -> Response -> IO ()
handleResponse sid state reader resp = do
               res <- evalConnectionM (writeResponse sid resp) (ConnStateConfig state reader)
               case res of 
                   Right _ -> return ()
                   Left _err -> undefined -- TODO send reset stream

instance NetworkMonad ConnectionM where
       writeBuffer bs = do
            conn <- asks stSocket
            liftIO $ SocketBS.sendAll conn $ BS.toStrict bs
       readBuffer = do
            buf <- gets stBuffer
            conn <- asks stSocket
            if BS.null buf
                then do 
                    buf2 <- liftIO $ BS.fromStrict <$> SocketBS.recv conn 1024
                    if BS.null buf2
                          then do
                             setStreamEnd
                             throwError EndOfStream
                          else return buf2
                else do
                    modify $ \s -> s { stBuffer = BS.empty }
                    return buf
