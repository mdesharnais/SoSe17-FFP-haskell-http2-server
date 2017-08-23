{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Main where

import Lib
import qualified Data.ByteString.Lazy as BS
import Data.Text

main :: IO ()
main = do
    result <- runServer $ ServerConfig 
                    { servHostname = "localhost"
                    , servPort = 8080
                    , servHandler = handler
                    }
    case result of
        Just e -> putStrLn $ "Error occured: " ++ show e
        Nothing -> putStrLn "Server stated"


handler :: Handler
handler Request { reqMethod, reqPath, reqDataChunk } = do
        case (reqMethod, reqPath) of 
             (HTTP_GET, "/") -> return Response 
                        { respStatus = 200
                        , respHeaders = [("content-type", "text/plain")]
                        , respData = ResponseComplete "Hello World!"
                        }
             _ -> return Response 
                     { respStatus = 404
                     , respHeaders = [("content-type", "text/plain")]
                     , respData = ResponseComplete "404"
                     }
              
