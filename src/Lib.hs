{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Lib 
 ( runServer
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
 ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as BS
import Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import qualified System.IO as IO

import qualified Connection

import Data.ByteString.Lazy(ByteString)
import Network.Socket(Socket, SockAddr(SockAddrInet))
import System.IO(stderr)
import Handler
import ServerConfig


h2ConnectionPrefix :: ByteString
h2ConnectionPrefix = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

showIPv4 :: Socket.HostAddress -> String -- TODO remove
showIPv4 addr =
  let (x, y, z, w) = Socket.hostAddressToTuple addr in
  show x ++ "." ++ show y ++ "." ++ show z ++ "." ++ show w

handleConnection :: Socket -> SockAddr -> ServerConfig -> IO () -- TODO umschreiben
handleConnection conn (SockAddrInet port addr) _ = do
  putStrLn $ "Incoming connection from " ++ showIPv4 addr ++ ":" ++ show port
  msg <- BS.fromStrict <$> SocketBS.recv conn (fromIntegral (BS.length h2ConnectionPrefix))
  if msg == h2ConnectionPrefix then do
    putStrLn $ "HTTP/2 Connection prefix received."
    Connection.handleConnection conn undefined -- TODO
  else do
    IO.hPutStr stderr "Invalid HTTP/2 connection prefix received: '"
    BS.hPutStr stderr msg -- Ungut besser hex condieren
    IO.hPutStrLn stderr "'" -- TODO sende HTTP 500 
handleConnection _ _ _ = return ()

run :: Socket -> ServerConfig -> IO () 
run s config =
  Monad.forever $ do
    (s1, sAddr) <- Socket.accept s
    Concurrent.forkIO (handleConnection s1 sAddr config >> Socket.close s1)

runServer :: ServerConfig ->  IO (Maybe ServerError) -- TODO use Bracket?
runServer config@ServerConfig{..} = do
       let hints = defaultHints 
                     { addrFlags = [AI_PASSIVE]
                     , addrSocketType = Stream
                     }
           hname = if null $ servHostname
                        then Nothing
                        else Just servHostname
           
       addrs <- Exception.handle catcher
                                    (Just <$> getAddrInfo (Just hints) hname (Just $ show servPort))
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
            catcher :: Exception.IOException -> IO (Maybe a)
            catcher _ = return Nothing
