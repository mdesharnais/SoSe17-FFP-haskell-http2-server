{-# LANGUAGE TypeFamilies #-}
module ConnHandler where

import Data.ByteString

class (Monad m) => ConnMonad m where
       type ConnHandler m :: *
       cGetSome :: ConnHandler m -> Int -> m ByteString
       cPut :: ConnHandler m -> ByteString -> m ()
       cFlush :: ConnHandler m -> m ()
       cClose :: ConnHandler m -> m ()

