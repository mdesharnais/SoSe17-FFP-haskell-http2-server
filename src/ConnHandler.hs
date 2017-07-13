module ConnHandler where

import Data.ByteString

class ConnHandler h where
       cGetSome :: h -> Int -> IO ByteString
       cPut :: h -> ByteString -> IO ()
       cFlush :: h -> IO ()
       cClose :: h -> IO ()

