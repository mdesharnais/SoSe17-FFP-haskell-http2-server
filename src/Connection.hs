{-# LANGUAGE FlexibleContexts #-}

module Connection(
  handleConnection
) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Set as Set

import qualified Frame

import Control.Monad.Except(ExceptT)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.State(StateT)
import Data.ByteString.Lazy(ByteString)

import Frame(Frame(..))
import ProjectPrelude

data ConnState = ConnState {
  stReadBuffer :: IO ByteString,
  stWriteBuffer :: ByteString -> IO (),
  stBuffer :: ByteString
}

type ConnectionM a = ExceptT ErrorCode (StateT ConnState IO) a

evalConnectionM :: ConnectionM a -> ConnState -> IO (Either ErrorCode a)
evalConnectionM conn = State.evalStateT (Except.runExceptT conn)

getBuffer :: ConnectionM (IO ByteString)
getBuffer = do
  buffer <- State.gets stBuffer
  State.modify (\state -> state { stBuffer = ByteString.empty })
  if ByteString.null buffer then do
    State.gets stReadBuffer
  else
    return (return buffer)

sendConnectionPreface :: ConnectionM ()
sendConnectionPreface = do
  writeBuffer <- State.gets stWriteBuffer
  liftIO $ Frame.writeFrame writeBuffer $ Frame {
    fLength = 0,
    fType = Frame.TSettings,
    fFlags = 0x0,
    fStreamId = StreamId 0,
    fPayload = Frame.PSettings Set.empty
  }

handleSettings :: Frame -> ConnectionM Bool
handleSettings Frame { fType = Frame.TSettings, fFlags = 0x0 } = do
  writeBuffer <- State.gets stWriteBuffer
  liftIO $ Frame.writeFrame writeBuffer $ Frame {
    fLength = 0,
    fType = Frame.TSettings,
    fFlags = 0x1,
    fStreamId = StreamId 0,
    fPayload = Frame.PSettings Set.empty
  }
  return True
handleSettings Frame { fType = Frame.TSettings, fFlags = 0x1 } = return True
handleSettings _ = undefined

handleFrame :: Frame -> ConnectionM Bool
handleFrame f = case Frame.fType f of
  Frame.TSettings -> handleSettings f
  _ -> return True

readFrame :: ConnectionM Frame
readFrame = do
  f <- getBuffer
  let impl (Get.Fail _ _ _)          = Except.throwError ProtocolError
      impl (Get.Partial continue)    = liftIO f >>= impl . continue . Just . ByteString.toStrict
      impl (Get.Done _ _ (Left err)) = Except.throwError err
      impl (Get.Done buffer _ (Right frame)) = do
        State.modify (\s -> s { stBuffer = ByteString.fromStrict buffer })
        return frame
  frame <- impl (Get.runGetIncremental (Except.runExceptT Frame.get))
  liftIO (putStrLn (Frame.toString frame))
  return frame

run :: ConnectionM ()
run = readFrame >>= handleFrame >>= flip Monad.when run

handleConnection :: IO ByteString -> (ByteString -> IO ()) -> IO ()
handleConnection stReadBuffer stWriteBuffer = do
  result <- evalConnectionM (sendConnectionPreface >> run) $ ConnState {
    stReadBuffer,
    stWriteBuffer,
    stBuffer = ByteString.empty
  }
  case result of
    Left  _ -> undefined
    Right _ -> return ()
