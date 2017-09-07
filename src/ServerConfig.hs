{-# LANGUAGE TypeFamilies #-}
module ServerConfig 
     ( ServerConfig (..)
     , ServerError (..)
     , ConnMode (..)
     , PlainConn
     , TLSConn
     , REither (..)
     , NMaybe (..)
     , ConnModeSocket
     ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Network.Socket.ByteString.Lazy as SocketBS
import Network.Socket (Socket)
import qualified Network.TLS as TLS

import ProjectPrelude
import Handler

class ConnMode a where
      modeSendAll :: ConnModeSocket a -> ByteString -> IO (NMaybe a)
      modeRecv :: ConnModeSocket a -> Int64 -> IO (REither a ByteString)

newtype REither a b = RRight b
newtype NMaybe a = NNothing ()

data PlainConn

data TLSConn

instance ConnMode PlainConn where
      modeSendAll sock bs = SocketBS.sendAll sock bs >> return (NNothing ())
      modeRecv sock count = SocketBS.recv sock count >>= return . RRight

instance ConnMode TLSConn where
      modeSendAll cont bs = TLS.sendData cont bs >> return (NNothing ())
      modeRecv cont _count = TLS.recvData cont >>= return . RRight . BS.fromStrict -- TODO ueberlege, ob count nicht notwendig ist

type family ConnModeConfig a :: *

type family ConnModeSocket a :: *

type instance ConnModeConfig PlainConn = ()

type instance ConnModeSocket PlainConn = Socket

type instance ConnModeConfig TLSConn = () -- TODO

type instance ConnModeSocket TLSConn = TLS.Context

data ServerConfig a = ServerConfig 
              { servHostname :: String
              , servPort :: Word16
              , servHandler :: Handler
              , servModeConfig :: ConnModeConfig a
              }

data ServerError = HostNotFound | BindFaild deriving Show
