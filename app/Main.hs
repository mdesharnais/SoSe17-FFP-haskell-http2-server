{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Main where

import Lib
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified System.IO as IO
import Data.Word

main :: IO ()
main = mainPlain

mainPlain :: IO ()
mainPlain = do
    result <- runServerPlain $ ServerConfig
                    { servHostname = "localhost"
                    , servPort = 8080
                    , servHandler = handler
                    , servModeConfig = ()
                    }
    case result of
        Just e -> putStrLn $ "Error occured: " ++ show e
        Nothing -> putStrLn "Server stated"

mainTLS :: IO ()
mainTLS = do
    result <- runServerTLS $ ServerConfig
                 { servHostname = "localhost"
                 , servPort = 8443
                 , servHandler = handler
                 , servModeConfig = TLSParams
                        { tlsCertificate = "../debug_ca/keys/myserver.crt"
                        , tlsPrivKey = "../debug_ca/keys/myserver.key"
                        }
                 }
    case result of
        Just e -> putStrLn $ "Error occured: " ++ show e
        Nothing -> putStrLn "Server stated"


handler :: Handler
handler Request { reqMethod, reqPath } = do
        case (reqMethod, reqPath) of
             (HTTP_GET, "/") -> return Response
                        { respStatus = 200
                        , respHeaders = [("content-type", "text/plain")]
                        , respData = ResponseComplete "Hello World!\n"
                        }
             (HTTP_GET, "/files/initrd.img.old") -> do
                     chuncks <- fileDataChuncked "/initrd.img.old"
                     return Response
                        { respStatus = 200
                        , respHeaders = [("content-type", "application/octet-stream")]
                        , respData = ResponseChunked chuncks
                        }
             (HTTP_GET, "/files/initrd.img") -> do
                     chuncks <- fileDataChuncked "/initrd.img"
                     return Response
                        { respStatus = 200
                        , respHeaders = [("content-type", "application/octet-stream")]
                        , respData = ResponseChunked chuncks
                        }
             _ -> return Response
                     { respStatus = 404
                     , respHeaders = [("content-type", "text/plain")]
                     , respData = ResponseComplete "404"
                     }


fileDataChuncked :: FilePath -> IO (Word32 -> IO ByteString)
fileDataChuncked file = do
             h <- IO.openFile file IO.ReadMode
             return $ handleDataChuncked h

handleDataChuncked :: IO.Handle -> (Word32 -> IO ByteString)
handleDataChuncked h count = do
             isOpen <- IO.hIsOpen h
             if isOpen
                then do
                   isEOF <- IO.hIsEOF h
                   if isEOF
                       then do
                          IO.hClose h
                          return BS.empty
                       else BS.hGet h (fromIntegral count)
                else return BS.empty
