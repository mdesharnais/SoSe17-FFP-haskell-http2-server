{-# LANGUAGE TypeFamilies #-}
module ConnHandler where

import Data.ByteString

class (Monad m) => ConnMonad m where
       type ConnHandler :: *
       cGetSome :: ConnHandler -> Int -> m ByteString
       cPut :: ConnHandler -> ByteString -> m ()
       cFlush :: ConnHandler -> m ()
       cClose :: ConnHandler -> m ()

