{-# LANGUAGE NamedFieldPuns #-}
module Streams
    ( PerStreamData (..)
    , StreamMonad (..)
    , StreamState (..)
    , initStreamsVar
    , initStreamData
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import ProjectPrelude
import Settings

class (Monad m) => StreamMonad m where
       newStream :: StreamId -> m ()
       streamAddHeaderFrag :: ByteString -> StreamId -> m ()
       getHeaders :: StreamId -> m ByteString
       getResvStrWindow :: StreamId -> m Int64
       getResvConnWindow :: m Int64
       getConnStrSendWindows :: StreamId -> m Int64
       fetchSubSendWindows :: StreamId -> Word32 -> m Word32
       addStrSendWindow :: StreamId -> Word32 -> m ()
       addConnSendWindow :: Word32 -> m ()
       fetchSubResvWindows :: StreamId -> Word32 -> m (Maybe (Word32,Word32))
       updateStrResvWindowTo :: StreamId -> Word32 -> m Word32
       updateConnResvWindowTo :: Word32 -> m Word32
       resvAction :: StreamId -> m (Int -> IO ByteString)
       execSendAction :: (Word32 -> IO ByteString) -> Word32 -> m ByteString
       getStreamState :: StreamId -> m StreamState
       setStreamState :: StreamId -> StreamState -> m ()
       resvData :: StreamId -> ByteString -> m ()


data StreamState = StreamIdle
                 | StreamClosed
                 | StreamRoot
                 | StreamOpen
                    { stStreamEnd :: Bool
                    , stHeaderEnd :: Bool
                    }
                 | StreamRst Endpoint
                deriving (Show, Eq)

data PerStreamData = PerStreamData
               { streamHeaderFragment :: TVar ByteString
               , resvChan :: TChan ByteString
               , resvWindow :: TVar Int64
               , sendWindow :: TVar Int64
               , streamState :: TVar StreamState
               }

initStreamData :: Word32 -> Word32 -> IO PerStreamData
initStreamData resvWin sendWin = do
          streamHeaderFragment <- newTVarIO BS.empty
          resvChan <- newTChanIO
          resvWindow <- newTVarIO $ fromIntegral resvWin
          sendWindow <- newTVarIO $ fromIntegral sendWin
          streamState <- newTVarIO StreamIdle
          return $ PerStreamData { streamHeaderFragment, resvChan,
                     resvWindow, sendWindow, streamState }

initRootStream :: IO PerStreamData
initRootStream = do
          headerFragment <- newTVarIO BS.empty
          rChan <- newTChanIO
          resvWindow <- newTVarIO $ initialWindowSize
          sendWindow <- newTVarIO $ initialWindowSize
          streamState <- newTVarIO StreamRoot
          return $ PerStreamData
                   { streamHeaderFragment = headerFragment
                   , resvChan = rChan
                   , resvWindow = resvWindow
                   , sendWindow = sendWindow
                   , streamState = streamState
                   }

initStreamsVar :: IO (Map StreamId PerStreamData)
initStreamsVar = do
             rootStream <- initRootStream
             return $ Map.singleton (StreamId 0) rootStream


