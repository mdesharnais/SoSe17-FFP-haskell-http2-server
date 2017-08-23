{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}
module ConnectionMonad ( ConnMonad(..)
                       , ConnectionM
                       , evalConnectionM
                       , ConnStateConfig
                       , initConnStateConfig
                       ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT, MonadError)
import qualified Control.Monad.Except as Except
import Network.Socket(Socket)
import Network.Socket.ByteString as SocketBS
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent (forkIO)


import Logger as Log
import ProjectPrelude
import Settings
import Streams
import Frame
import ServerConfig
import ConnMonad
import Handler

data ConnStateConfig = ConnStateConfig ConnState ConnReader

data ConnState = ConnState 
               { stBuffer :: ByteString
               , stMaxStreamId :: StreamId
               , stStreams  :: Map StreamId PerStreamData
               , stExpectMoreHeaders :: Maybe (StreamId)
               }

data ConnReader = ConnReader
               { stSettings :: TVar ConnSettings
               , stSendChan :: TChan Frame
               , stServerConfig :: ServerConfig
               , stSocket :: Socket
               }


newtype ConnectionM a = ConnectionM (ExceptT ErrorCode (RWST ConnReader () ConnState IO) a) 
                                    deriving ( Functor
                                             , Applicative
                                             , Monad
                                             , MonadError ErrorCode
                                             , MonadState ConnState
                                             , MonadReader ConnReader
                                             , MonadIO
                                             )

instance Log.Logger ConnectionM where
      log = liftIO `oo` Log.log

instance MonadSetting ConnectionM where
       getSettings = do
               settings <- asks stSettings
               liftIO $ readTVarIO settings
       getSetting f = do
               settings <- asks stSettings 
               liftIO $ fmap f $ readTVarIO settings
       modifySettings f = do
               settings <- asks stSettings
               liftIO $ atomically $ modifyTVar settings f

instance StreamMonad ConnectionM where
      -- newStream :: StreamId -> m ()
      newStream sid@(StreamId sid') = do
             (StreamId maxSid) <- gets stMaxStreamId
             when (sid' <= maxSid || even sid') $ throwError ProtocolError -- connection error
             modify $ \s -> s { stMaxStreamId = sid }
             settingVar <- asks stSettings
             settings <- liftIO $ readTVarIO settingVar
             streams <- gets stStreams
             when (fromIntegral (getMaxConcurrentStreams LocalEndpoint settings) >= Map.size streams) -- TODO reserved streams dont count
                        $ throwError ProtocolError -- stream error or refused stream
             let localInitWindow = getInitialWindowSize LocalEndpoint settings
                 remoteInitWindow = getInitialWindowSize RemoteEndpoint settings
             streamData <- liftIO $ initStreamData localInitWindow remoteInitWindow
             modify $ \s -> s { stStreams = Map.insert sid streamData (stStreams s) }
      -- streamAddHeaderFrag :: ByteString -> StreamId -> m ()
      streamAddHeaderFrag bs sid = do
            streamData <- getStreamData sid
            liftIO $ atomically $ modifyTVar (streamHeaderFragment streamData) $ \headers -> BS.append headers bs
      -- getHeaders :: StreamId -> m ByteString
      getHeaders sid = do
              streamData <- getStreamData sid
              liftIO $ atomically $ do
                let fragment = streamHeaderFragment streamData
                headers <- readTVar fragment
                writeTVar fragment BS.empty
                return headers
      -- getResvWindow :: StreamId -> m Word32
      getResvWindow sid = do
              streamData <- getStreamData sid
              liftIO $ readTVarIO (resvWindow streamData)
      -- getSendWindow :: StreamId -> m Word32
      getSendWindow sid = do
              streamData <- getStreamData sid
              liftIO $ readTVarIO (sendWindow streamData)
      -- resvData :: StreamId -> ByteString -> m () -- TODO noetig ??
      resvData = undefined
      -- resvAction :: StreamId -> m (Int -> IO ByteString)
      resvAction = undefined -- TODO
      -- execSendAction :: StreamId -> (Int -> IO ByteString) -> m () -- TODO noetig ??
      execSendAction = undefined
      -- getStreamState :: StreamId -> m StreamState
      getStreamState sid = do
              streamData <- getStreamData sid
              liftIO $ readTVarIO (streamState streamData)
      -- setStreamState :: StreamId -> StreamState -> m ()
      setStreamState sid newState = do
              streamData <- getStreamData sid
              oldState <- liftIO $ readTVarIO (streamState streamData)
              if checkStateTrans oldState newState
                 then liftIO $ atomically $ writeTVar (streamState streamData) newState
                 else throwError ProtocolError -- streamError

getStreamData :: StreamId -> ConnectionM PerStreamData
getStreamData sid = do
        streamData <- gets $ (Map.lookup sid) . stStreams
        case streamData of
            Nothing -> throwError ProtocolError -- Connection Error
            Just sData -> return sData

checkStateTrans :: StreamState -> StreamState -> Bool
checkStateTrans StreamIdle (StreamOpen {}) = True
checkStateTrans (StreamOpen {}) StreamClosed = True
checkStateTrans (StreamOpen { stHeaderEnd = False, stStreamEnd = end1 }) (StreamOpen { stHeaderEnd = True, stStreamEnd = end2 }) = 
                                              end1 == end2
checkStateTrans (StreamOpen { stHeaderEnd = True, stStreamEnd = False }) (StreamOpen { stHeaderEnd = True, stStreamEnd = True }) = 
                                              True
checkStateTrans s1 s2 = s1 == s2


instance ConnMonad ConnectionM where
       writeBuffer bs = do
            conn <- asks stSocket
            liftIO $ SocketBS.sendAll conn $ BS.toStrict bs
       readBuffer = do
            buf <- gets stBuffer
            conn <- asks stSocket
            if BS.null buf
                then do 
                    liftIO $ BS.fromStrict <$> SocketBS.recv conn 1024
                else do
                    modify $ \s -> s { stBuffer = BS.empty }
                    return buf
       pushBackBuffer bs = do
            buffer <- gets stBuffer
            modify $ \s -> s { stBuffer = BS.append bs buffer }
       throwError err = Except.throwError err
       runHandler = runHandlerImpl
{-
       runHandler req = do 
            handler <- asks $ servHandler . stServerConfig
            _resp <- liftIO $ handler req
            undefined -- TODO forkIO und response zurueckschreiben
-}
       -- moreHeadersExpected :: m (Maybe StreamId)
       moreHeadersExpected = gets stExpectMoreHeaders
       -- setMoreHeaders :: (Maybe StreamId) -> m ()
       setMoreHeaders mh = modify $ \s -> s { stExpectMoreHeaders = mh }


runHandlerImpl :: Request -> ConnectionM ()
runHandlerImpl req = do -- TODO forkIO und response schreiben
        handler <- asks $ servHandler . stServerConfig
        _ <- liftIO $ forkIO $ handler req >>= writeResponse -- TODO catch expection
        return ()
     
writeResponse :: Response -> IO ()
writeResponse = undefined -- TODO

initConnStateConfig :: Socket -> ServerConfig -> IO ConnStateConfig
initConnStateConfig sock config = do
                          reader <- initConnReader sock config
                          state <- initConnState
                          return $ ConnStateConfig state reader
                      

initConnState :: IO ConnState
initConnState = do
               streams <- initStreamsVar
               return $ ConnState 
                       { stBuffer = BS.empty
                       , stMaxStreamId = StreamId 0
                       , stStreams = streams
                       , stExpectMoreHeaders = Nothing
                       }

initConnReader :: Socket -> ServerConfig -> IO ConnReader
initConnReader sock config = do
               settings <- newTVarIO initConnSettings
               sendChan <- newTChanIO
               return $ ConnReader
                        { stSettings = settings
                        , stSendChan = sendChan
                        , stSocket = sock
                        , stServerConfig = config
                        }

evalConnectionM :: ConnectionM a -> ConnStateConfig -> IO (Either ErrorCode a)
evalConnectionM (ConnectionM conn) (ConnStateConfig state config) = do
                          (a, _) <- evalRWST (Except.runExceptT $ conn) config state
                          return a
           
