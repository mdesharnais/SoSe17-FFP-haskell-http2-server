module Settings
           ( ConnSettings
           , MonadSetting (..)
           , initConnSettings
           , setHeaderTableSize
           , setEnablePush
           , setMaxConcurrentStreams
           , setInitialWindowSize
           , setMaxFrameSize
           , setMaxHeaderListSize
           , getHeaderTableSize
           , getEnablePush
           , getMaxConcurrentStreams
           , getInitialWindowSize
           , getMaxFrameSize
           , getMaxHeaderListSize
           , initialWindowSize
           , maximalWindowSize
           ) where

import ProjectPrelude

data SettingPair a = SettingPair { setLocal :: a
                                 , setPeer  :: a
                                 }

data ConnSettings = ConnSettings
             { settHeaderTableSize :: SettingPair Word32
             , settEnablePush      :: SettingPair Bool
             , settMaxConcurrentStreams :: SettingPair Word32
             , settInitialWindowSize :: SettingPair Word32
             , settMaxFrameSize  :: SettingPair Word32
             , settMaxHeaderListSize :: SettingPair Word32 -- Ignored because unclear
             }

class Monad m => MonadSetting m where
        getSettings  :: m ConnSettings
        getSetting   :: (ConnSettings -> a) -> m a
        modifySettings :: (ConnSettings -> ConnSettings) -> m ()
        adjustRemoteWindowSize :: Int64 -> Word32 -> m ()

initialWindowSize :: (Num a) => a
initialWindowSize = 2 ^ (16::Int) - 1

maximalWindowSize :: (Num a) => a
maximalWindowSize = 2 ^ (31 :: Int) - 1

initSetPair :: a -> SettingPair a
initSetPair a = SettingPair { setLocal = a
                            , setPeer = a
                            }

getSetPairField :: Endpoint -> SettingPair a -> a
getSetPairField LocalEndpoint (SettingPair {setLocal=a} ) = a
getSetPairField RemoteEndpoint (SettingPair {setPeer=a} ) = a

setSetPairField :: Endpoint -> a -> SettingPair a -> SettingPair a
setSetPairField LocalEndpoint a s = s { setLocal=a }
setSetPairField RemoteEndpoint a s = s { setPeer=a }

initConnSettings :: ConnSettings
initConnSettings = ConnSettings
             { settHeaderTableSize = initSetPair 4096
             , settEnablePush = initSetPair True
             , settMaxConcurrentStreams = initSetPair maxBound
             , settInitialWindowSize = initSetPair initialWindowSize
             , settMaxFrameSize = initSetPair $ (2 ^ (14::Int))
             , settMaxHeaderListSize = initSetPair maxBound
             }

setHeaderTableSize :: Endpoint -> Word32 -> ConnSettings -> ConnSettings
setHeaderTableSize endp a sett = sett { settHeaderTableSize = setSetPairField endp a $ settHeaderTableSize sett }

setEnablePush :: Endpoint -> Bool -> ConnSettings -> ConnSettings
setEnablePush endp a sett = sett { settEnablePush = setSetPairField endp a $ settEnablePush sett }

setMaxConcurrentStreams :: Endpoint -> Word32 -> ConnSettings -> ConnSettings
setMaxConcurrentStreams endp a sett = sett { settMaxConcurrentStreams = setSetPairField endp a $ settMaxConcurrentStreams sett }

setInitialWindowSize :: Endpoint -> Word32 -> ConnSettings -> ConnSettings
setInitialWindowSize endp a sett = sett { settInitialWindowSize = setSetPairField endp a $ settInitialWindowSize sett }

setMaxFrameSize :: Endpoint -> Word32 -> ConnSettings -> ConnSettings
setMaxFrameSize endp a sett = sett { settMaxFrameSize = setSetPairField endp a $ settMaxFrameSize sett }

setMaxHeaderListSize :: Endpoint -> Word32 -> ConnSettings -> ConnSettings
setMaxHeaderListSize endp a sett = sett { settMaxHeaderListSize = setSetPairField endp a $ settMaxHeaderListSize sett }


getHeaderTableSize :: Endpoint -> ConnSettings -> Word32
getHeaderTableSize endp sett = getSetPairField endp $ settHeaderTableSize sett

getEnablePush :: Endpoint -> ConnSettings -> Bool
getEnablePush endp sett = getSetPairField endp $ settEnablePush sett

getMaxConcurrentStreams :: Endpoint -> ConnSettings -> Word32
getMaxConcurrentStreams endp sett = getSetPairField endp $ settMaxConcurrentStreams sett

getInitialWindowSize :: Endpoint -> ConnSettings -> Word32
getInitialWindowSize endp sett = getSetPairField endp $ settInitialWindowSize sett

getMaxFrameSize :: Endpoint -> ConnSettings -> Word32
getMaxFrameSize endp sett = getSetPairField endp $ settMaxFrameSize sett

getMaxHeaderListSize :: Endpoint -> ConnSettings -> Word32
getMaxHeaderListSize endp sett = getSetPairField endp $ settMaxHeaderListSize sett

