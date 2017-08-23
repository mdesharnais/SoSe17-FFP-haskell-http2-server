{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}

module Connection(
  handleConnection
) where

import Control.Monad (when)
import qualified Control.Monad.Except as Except
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Set as Set
import qualified Data.Text as Text

import Frame
import qualified Frame.Settings as FSettings

import qualified Handle.Headers as HHeaders
import qualified Handle.Settings as HSettings
import qualified Handle.Data as HData ()

import Network.Socket(Socket)

import Frame(Frame(..))
import ProjectPrelude
import ConnectionMonad
import qualified Logger
import ServerConfig

sendConnectionPreface :: (ConnMonad m) => m () -- TODO 
sendConnectionPreface = do
  writeBuffer $ Frame.writeFrame $ Frame {
    fFlags = 0x0,
    fStreamId = StreamId 0,
    fPayload = Frame.PSettings $ Set.singleton 
                 (FSettings.EnablePush, 0)
  }


handleFrame :: (ConnMonad m) => Frame -> m Bool
handleFrame (Frame {{-fType,-} fPayload, fStreamId, fFlags}) = do
 expHeaders <- moreHeadersExpected 
 case (expHeaders, fPayload)  of
      (Nothing, _) -> return ()
      (Just streamid, PContinuation _ ) | streamid == fStreamId -> return ()
      _ -> throwError ProtocolError -- TODO Aendern entsprechend position von catchError
 case fPayload of
   (PSettings _) -> HSettings.handleSettings fPayload fStreamId fFlags -- TODO
   (PHeaders payload) -> HHeaders.handleHeaders payload fStreamId fFlags
   (PContinuation payload) -> HHeaders.handleContinuation payload fStreamId fFlags
   _ -> return True

readFrame :: (ConnMonad m) => m Frame
readFrame = do
  let impl (Get.Fail _ _ _)          = throwError ProtocolError
      impl (Get.Partial continue)    = readBuffer >>= impl . continue . Just . ByteString.toStrict
      impl (Get.Done _ _ (Left err)) = throwError err
      impl (Get.Done buffer _ (Right frame)) = do
        pushBackBuffer $ ByteString.fromStrict buffer
        return frame
  frame <- impl (Get.runGetIncremental (Except.runExceptT Frame.get))
  Logger.log Logger.Info $ Text.pack (Frame.toString frame)
  return frame

-- TODO BEGIN eigenen Sender und Resiver Thread. Beide konform zu Connection Preface. 
-- D.h. Resiver erwartet Settings
run :: (ConnMonad m) => m () 
run = readFrame >>= handleFrame >>= flip when run

handleConnection :: Socket -> ServerConfig -> IO ()
handleConnection sock config = do
  stateConfig <- initConnStateConfig sock config
  result <- evalConnectionM (sendConnectionPreface >> run) stateConfig
  case result of
    Left  _ -> undefined
    Right _ -> return () 
-- TODO END
