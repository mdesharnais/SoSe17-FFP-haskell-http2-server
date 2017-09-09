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
import Control.Monad.Except (ExceptT, MonadError)
import qualified Control.Monad.Except as Except
import Control.Monad.RWS

import ProjectPrelude
import Streams
import Settings
import Frame (Frame)
import ErrorCodes
import ServerConfig
import Hpack

data ConnStateConfig mode = ConnStateConfig ConnState (ConnReader mode)

data ConnState = ConnState
               { stBuffer :: ByteString
               , stMaxStreamId :: StreamId
               , stStreams  :: Map StreamId PerStreamData
               , stExpectMoreHeaders :: Maybe (StreamId)
               , stLocalDynTable :: DynamicTable
               , stRemoteDynTable :: DynamicTable
               }

data ConnReader mode = ConnReader
               { stSettings :: TVar ConnSettings
               , stSendChan :: TChan Frame
               , stEndStream :: TVar Bool
               , stServerConfig :: ServerConfig mode
               , stConnSendWindow :: TVar Int64
               , stConnResvWindow :: TVar Int64
               , stSocket :: ConnModeSocket mode
               }


newtype ConnectionM mode a = ConnectionM (ExceptT ConnError (RWST (ConnReader mode) () ConnState IO) a)
                                    deriving ( Functor
                                             , Applicative
                                             , Monad
                                             , MonadError ConnError
                                             , MonadState ConnState
                                             , MonadReader (ConnReader mode)
                                             , MonadIO
                                             )

initConnStateConfig :: ConnModeSocket mode -> ServerConfig mode -> IO (ConnStateConfig mode)
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
                       , stLocalDynTable = emptyDynTable
                       , stRemoteDynTable = emptyDynTable
                       }

initConnReader :: ConnModeSocket mode -> ServerConfig mode -> IO (ConnReader mode)
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

evalConnectionM :: ConnectionM mode a -> ConnStateConfig mode -> IO (Either ConnError a)
evalConnectionM (ConnectionM conn) (ConnStateConfig state config) = do
                          (a, _) <- evalRWST (Except.runExceptT $ conn) config state
                          return a
