module Lib (
  someFunc
) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Network.Socket as Socket

import Network.Socket(Socket, SockAddr(SockAddrInet))

localhost :: Socket.HostAddress
localhost = Socket.tupleToHostAddress (127, 0, 0, 1)

showIPv4 :: Socket.HostAddress -> String
showIPv4 addr =
  let (x, y, z, w) = Socket.hostAddressToTuple addr in
  show x ++ "." ++ show y ++ "." ++ show z ++ "." ++ show w

handleConnection :: Socket -> SockAddr -> IO ()
handleConnection s (SockAddrInet port addr) = putStrLn $ showIPv4 addr ++ ":" ++ show port
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
  s <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  flip Exception.finally (cleanup s) $ do
    Socket.bind s $ SockAddrInet 8080 localhost
    Socket.listen s 1
    run s
