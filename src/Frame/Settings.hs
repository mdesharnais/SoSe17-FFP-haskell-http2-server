module Frame.Settings(
  Param,
  Payload,
  Setting(..),
  getPayload,
  toString
)where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Data.Binary.Get as Get
import qualified Data.Set as Set

import Control.Monad.Except(ExceptT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Set(Set)
import Data.Word(Word16, Word32)

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
  value <- Get.getWord16be
  return $ case value of
    0x1 -> HeaderTableSize
    0x2 -> EnablePush
    0x3 -> MaxConcurrentStreams
    0x4 -> InitialWindowSize
    0x5 -> MaxFrameSize
    0x6 -> MaxHeaderListSize
    _   -> Unknown value

getParam :: Get Param
getParam = (,) <$> getSetting <*> Get.getWord32be

getPayload :: FrameLength -> FrameFlags -> StreamId -> ExceptT ErrorCode Get Payload
getPayload len _ _ =
  let (q, m) = divMod len 6 in
  if m /= 0 then
    Except.throwError FrameSizeError
  else
    lift $ Set.fromList <$> Monad.replicateM (fromIntegral q) getParam

toString :: String -> Payload -> String
toString prefix = Set.foldr (\(k, v) acc -> prefix ++ show k ++ " = " ++ show v ++ "\n" ++ acc) ""
