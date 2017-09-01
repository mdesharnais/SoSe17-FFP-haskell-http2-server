module Handle.WindowUpdate 
  ( handleWindowUpdate
  , sendStreamWindowUpdate
  , sendConnWindowUpdate
  , sendWindowUpdates
  ) where

import Control.Monad (when)

import Frame.WindowUpdate as FWindowUpdate
import ProjectPrelude
import ConnMonad
import Streams
import Settings
import Frame (Frame (..), Payload (..))

handleWindowUpdate :: (ConnMonad m) => FWindowUpdate.Payload -> StreamId -> FrameFlags -> m ()
handleWindowUpdate update sid _flags = do
              if sid == (StreamId 0)
                    then addStrSendWindow sid update 
                    else addConnSendWindow update

sendStreamWindowUpdate :: (ConnMonad m) => StreamId -> m ()
sendStreamWindowUpdate sid = do
                defaultWin <- getSetting $ getInitialWindowSize LocalEndpoint
                update <- updateStrResvWindowTo sid defaultWin
                let frame = Frame { fPayload = PWindowUpdate update, fStreamId = sid, fFlags = 0 }
                when (update > 0) $ sendFrame frame

sendConnWindowUpdate :: (ConnMonad m) => m ()
sendConnWindowUpdate = do
               defaultWin <- getSetting $ getInitialWindowSize LocalEndpoint
               update <- updateConnResvWindowTo defaultWin
               let frame = Frame { fPayload = PWindowUpdate update, fStreamId = StreamId 0, fFlags = 0 }
               when (update > 0) $ sendFrame frame

sendWindowUpdates :: (ConnMonad m) => StreamId -> m ()
sendWindowUpdates sid= do
               sendStreamWindowUpdate sid
               sendConnWindowUpdate
