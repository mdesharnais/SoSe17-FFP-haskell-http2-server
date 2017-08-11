module Frame.Settings(
  Param,
  Payload,
  Setting(..),
  getPayload,
  putPayload,
  toString
)where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set

import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.Set(Set)

import ProjectPrelude

data Setting =
  HeaderTableSize |
  EnablePush |
  MaxConcurrentStreams |
  InitialWindowSize |
  MaxFrameSize |
  MaxHeaderListSize |
  Unknown Word16
  deriving (Eq, Ord, Show)

type Param = (Setting, Word32)
type Payload = Set Param

getSetting :: Get Setting
getSetting = do
  code <- Get.getWord16be
  return $ case code of
    0x1 -> HeaderTableSize
    0x2 -> EnablePush
    0x3 -> MaxConcurrentStreams
    0x4 -> InitialWindowSize
    0x5 -> MaxFrameSize
    0x6 -> MaxHeaderListSize
    _   -> Unknown code

putSetting :: Setting -> Put
putSetting HeaderTableSize      = Put.putWord16be 0x1
putSetting EnablePush           = Put.putWord16be 0x2
putSetting MaxConcurrentStreams = Put.putWord16be 0x3
putSetting InitialWindowSize    = Put.putWord16be 0x4
putSetting MaxFrameSize         = Put.putWord16be 0x5
putSetting MaxHeaderListSize    = Put.putWord16be 0x6
putSetting (Unknown code)       = Put.putWord16be code

getParam :: Get Param
getParam = (,) <$> getSetting <*> Get.getWord32be

putParam :: Param -> Put
putParam (setting, value) = putSetting setting >> Put.putWord32be value

getPayload :: FrameLength -> FrameFlags -> StreamId -> ExceptT ErrorCode Get Payload
getPayload len _ _ =
  let (q, m) = divMod len 6 in
  if m /= 0 then
    Except.throwError FrameSizeError
  else
    lift $ Set.fromList <$> Monad.replicateM (fromIntegral q) getParam

putPayload :: Payload -> Put
putPayload = Foldable.traverse_ putParam

toString :: String -> Payload -> String
toString prefix = Set.foldr (\(k, v) acc -> prefix ++ show k ++ " = " ++ show v ++ "\n" ++ acc) ""
