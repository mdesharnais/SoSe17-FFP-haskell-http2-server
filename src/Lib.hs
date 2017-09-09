{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}

module Lib
 ( runServerPlain
 , runServerTLS
 , ServerConfig (..)
 , ServerError (..)
 , HTTPMethod (..)
 , Request (..)
 , Response (..)
 , Handler
 , Headers
 , HeaderName
 , HeaderValue
 , HeaderField
 , ResponseData (..)
 , TLSParams (..)
 ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Network.Socket as Socket
import Numeric
import Data.String (fromString)
import qualified Data.Default.Class as Default
import qualified Data.ByteString as SBS

import qualified Connection

import Network.Socket(Socket, SockAddr(SockAddrInet))
import Handler
import ServerConfig
import qualified Logger as Log
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as Cipher



showIPAddr :: SockAddr -> String
showIPAddr (SockAddrInet port addr) =
       let (x,y,z,w) = Socket.hostAddressToTuple addr in
       show x ++ "." ++ show y ++ "." ++ show z ++ "." ++ show w ++ ":" ++ show port
showIPAddr (SockAddrInet6 port _ addr _) =
       let (b1,b2,b3,b4,b5,b6,b7,b8) = Socket.hostAddress6ToTuple addr in
       "[" ++ showHex b1 ":" ++ showHex b2 ":" ++ showHex b3 ":" ++ showHex b4 ":"
           ++ showHex b5 ":" ++ showHex b6 ":" ++ showHex b7 ":" ++ showHex b8 "]"
           ++ ":" ++ show port
showIPAddr _ = "Unsupported protocol"

handleConnection :: forall mode . (ConnMode mode) => ConnModeSocket mode -> SockAddr -> ServerConfig mode -> IO ()
handleConnection conn sockaddr config = do
  Log.log Log.Info $ fromString $ "Incoming connection from " ++ showIPAddr sockaddr
  Connection.handleConnection conn config


resolveName :: ServerConfig mode -> IO (Maybe [AddrInfo])
resolveName ServerConfig{..} = do
       let hints = defaultHints
                     { addrFlags = [AI_PASSIVE]
                     , addrSocketType = Stream
                     }
           hname = if null $ servHostname
                        then Nothing
                        else Just servHostname

       Exception.handle catcher (Just <$> getAddrInfo (Just hints) hname (Just $ show servPort))
       where catcher :: Exception.IOException -> IO (Maybe a)
             catcher _ = return Nothing

runServer :: (ConnMode mode) => (Socket -> ServerConfig mode -> IO()) -> ServerConfig mode ->  IO (Maybe ServerError)
runServer run config = do
       addrs <- resolveName config
       case addrs of
           Just (addr:_) -> Exception.bracket (createSocket addr) closeSocket $ \sock -> do
                                                   bindres <- Exception.try (bindlisten addr sock)
                                                                 :: IO (Either Exception.IOException ())
                                                   case bindres of
                                                           Left _ -> return $ Just BindFaild
                                                           Right _ -> run sock config >> return Nothing
           _ -> return $ Just HostNotFound
      where createSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bindlisten addr sock = do
                              bind sock (addrAddress addr)
                              listen sock 100
            closeSocket sock = Socket.close sock

runServerPlain :: ServerConfig PlainConn -> IO (Maybe ServerError)
runServerPlain = runServer runPlain

runPlain :: Socket -> ServerConfig PlainConn -> IO ()
runPlain s config =
  Monad.forever $ do
    (s1, sAddr) <- Socket.accept s
    Concurrent.forkIO (handleConnection s1 sAddr config >> Socket.close s1)

runServerTLS :: ServerConfig TLSConn -> IO (Maybe ServerError)
runServerTLS = runServer runTLS

runTLS :: Socket -> ServerConfig TLSConn -> IO ()
runTLS s config = do
  eParams <- tlsServerParams config
  case eParams of
     Left err -> do
          Log.log Log.Crit "Error while initialising TLS params"
          Log.log Log.Crit $ fromString err
     Right params -> Monad.forever $ do
          (s1, sAddr) <- Socket.accept s
          context <- TLS.contextNew (TLS.getBackend s1) params
          TLS.handshake context
          Concurrent.forkIO (handleConnection context sAddr config >> TLS.contextClose context)

tlsServerShared :: ServerConfig TLSConn -> IO (Either String TLS.Shared)
tlsServerShared config = do
               eCredent <- TLS.credentialLoadX509 (tlsCertificate $ servModeConfig config) (tlsPrivKey $ servModeConfig config)
               case eCredent of
                     Left err -> return $ Left err
                     Right credent -> return . Right $ Default.def
                               { TLS.sharedCredentials = TLS.Credentials [credent] }

tlsServerParams :: ServerConfig TLSConn -> IO (Either String TLS.ServerParams)
tlsServerParams config = do
              eShared <- tlsServerShared config
              case eShared of
                 Left err -> return $ Left err
                 Right shared -> return . Right $ Default.def
                    { TLS.serverShared = shared
                    , TLS.serverHooks = tlsServerHooks shared
                    , TLS.serverSupported = tlsSupported
                    }

tlsSupported :: TLS.Supported
tlsSupported = Default.def { TLS.supportedVersions = [TLS.TLS12]
                           , TLS.supportedCiphers = [Cipher.cipher_AES256GCM_SHA384]
                           , TLS.supportedCompressions = [TLS.nullCompression]
                           , TLS.supportedHashSignatures = [(TLS.HashSHA384, TLS.SignatureRSA), (TLS.HashSHA512, TLS.SignatureRSA)]
                     }

tlsServerHooks :: TLS.Shared -> TLS.ServerHooks
tlsServerHooks shared = Default.def { TLS.onServerNameIndication = \_ -> return $ TLS.sharedCredentials shared
                             , TLS.onSuggestNextProtocols = return $ Just ["h2"]
                             , TLS.onALPNClientSuggest = Just tlsALPNSuggest
                            }

tlsALPNSuggest :: [SBS.ByteString] -> IO SBS.ByteString
tlsALPNSuggest protos = if elem "h2" protos
                           then return "h2"
                           else return ""
