{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ConnectionM 
  ( ConnStateConfig (..)
  , ConnectionM
  , ConnState (..)
  , ConnReader (..)
  , evalConnectionM
  , initConnStateConfig
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map.Strict (Map)
import Control.Concurrent.STM
import Network.Socket (Socket)
import Control.Monad.Except (ExceptT, MonadError)
import qualified Control.Monad.Except as Except
import Control.Monad.RWS

import ProjectPrelude
import Streams
import Settings
import Frame (Frame)
import ErrorCodes
import ServerConfig

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
               , stEndStream :: TVar Bool
               , stServerConfig :: ServerConfig
               , stConnSendWindow :: TVar Int64
               , stConnResvWindow :: TVar Int64
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
initConnReader stSocket stServerConfig = do
               stSettings <- newTVarIO initConnSettings
               stSendChan <- newTChanIO
               stEndStream <- newTVarIO False
               stConnSendWindow <- newTVarIO initialWindowSize
               stConnResvWindow <- newTVarIO initialWindowSize
               return $ ConnReader
                        { stSettings
                        , stSendChan
                        , stSocket
                        , stServerConfig
                        , stEndStream
                        , stConnSendWindow
                        , stConnResvWindow
                        }

evalConnectionM :: ConnectionM a -> ConnStateConfig -> IO (Either ErrorCode a)
evalConnectionM (ConnectionM conn) (ConnStateConfig state config) = do
                          (a, _) <- evalRWST (Except.runExceptT $ conn) config state
                          return a
