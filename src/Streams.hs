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

class (Monad m) => StreamMonad m where
       newStream :: StreamId -> m ()
       streamAddHeaderFrag :: ByteString -> StreamId -> m ()
       getHeaders :: StreamId -> m ByteString
       getResvWindow :: StreamId -> m Word32
       getSendWindow :: StreamId -> m Word32
       resvData :: StreamId -> ByteString -> m () -- TODO noetig ??
       resvAction :: StreamId -> m (Int -> IO ByteString)
       execSendAction :: StreamId -> (Int -> IO ByteString) -> m ()
       getStreamState :: StreamId -> m StreamState
       setStreamState :: StreamId -> StreamState -> m ()
       
           
data StreamState = StreamIdle
                 | StreamClosed
                 | StreamRoot
                 | StreamOpen 
                    { stStreamEnd :: Bool
                    , stHeaderEnd :: Bool
                    }
                deriving (Show, Eq)
 
--data StreamState = StreamHeaderResv | StreamHeaderEnd | StreamEnd | StreamRoot | StreamIdle deriving (Show, Eq)

data PerStreamData = PerStreamData
               { streamHeaderFragment :: TVar ByteString
               , resvChan :: TChan ByteString
               , resvWindow :: TVar Word32
               , sendWindow :: TVar Word32
               , streamState :: TVar StreamState
               }

initStreamData :: Word32 -> Word32 -> IO PerStreamData
initStreamData resvWin sendWin = do
          streamHeaderFragment <- newTVarIO BS.empty
          resvChan <- newTChanIO
          resvWindow <- newTVarIO resvWin
          sendWindow <- newTVarIO sendWin
          streamState <- newTVarIO StreamIdle
          return $ PerStreamData { streamHeaderFragment, resvChan, 
                     resvWindow, sendWindow, streamState }

initRootStream :: IO PerStreamData
initRootStream = do
          headerFragment <- newTVarIO BS.empty
          rChan <- newTChanIO
          resvWindow <- newTVarIO $ (2 ^ (16::Int)) - 1 -- initial Window Size
          sendWindow <- newTVarIO $ (2 ^ (16::Int)) - 1
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


