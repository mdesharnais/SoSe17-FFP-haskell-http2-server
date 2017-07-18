{-# LANGUAGE OverloadedStrings #-}

module Lib (
  someFunc
) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as BS
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS
import qualified System.IO as IO

import qualified Connection

import Data.ByteString.Lazy(ByteString)
import Network.Socket(Socket, SockAddr(SockAddrInet))
import System.IO(stderr)

h2ConnectionPrefix :: ByteString
h2ConnectionPrefix = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

localhost :: Socket.HostAddress
localhost = Socket.tupleToHostAddress (127, 0, 0, 1)

showIPv4 :: Socket.HostAddress -> String
showIPv4 addr =
  let (x, y, z, w) = Socket.hostAddressToTuple addr in
  show x ++ "." ++ show y ++ "." ++ show z ++ "." ++ show w

handleConnection :: Socket -> SockAddr -> IO ()
handleConnection conn (SockAddrInet port addr) = do
  putStrLn $ "Incoming connection from " ++ showIPv4 addr ++ ":" ++ show port
  msg <- BS.fromStrict <$> SocketBS.recv conn (fromIntegral (BS.length h2ConnectionPrefix))
  if msg == h2ConnectionPrefix then do
    putStrLn $ "HTTP/2 Connection prefix received."
    let readBuffer = BS.fromStrict <$> SocketBS.recv conn 1024
    let writeBuffer = SocketBS.sendAll conn . BS.toStrict
    Connection.handleConnection readBuffer writeBuffer
  else do
    IO.hPutStr stderr "Invalid HTTP/2 connection prefix received: '"
    BS.hPutStr stderr msg
    IO.hPutStrLn stderr "'"
handleConnection _ _ = return ()

run :: Socket -> IO ()
run s =
  Monad.forever $ do
    (s1, sAddr) <- Socket.accept s
    Concurrent.forkIO (handleConnection s1 sAddr >> Socket.close s1)

cleanup :: Socket -> IO ()
cleanup s = do
  putStr "Closing socket..."
  Socket.close s
  putStrLn " Done."

someFunc :: IO ()
someFunc = do
  putStr $ "Opening socket..."
  s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  putStrLn " Done."
  flip Exception.finally (cleanup s) $ do
    Socket.bind s $ SockAddrInet 8080 localhost
    Socket.listen s 1
    putStrLn $ "Listening on " ++ showIPv4 localhost ++ ":8080"
    run s
