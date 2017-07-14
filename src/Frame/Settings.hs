module Frame.Settings(
  Param,
  Payload,
  Setting(..),
  getPayload,
  toString
)where

import qualified Data.Binary.Get as Get
import qualified Data.Set as Set

import qualified Control.Monad as Monad
import qualified Data.Set as Set

import Data.Binary.Get(Get)
import Data.Set(Set)
import Data.Word(Word8, Word32)

import ProjectPrelude

data Setting =
  HeaderTableSize |
  EnablePush |
  MaxConcurrentStreams |
  InitialWindowSize |
  MaxFrameSize |
  MaxHeaderListSize
  deriving (Eq, Ord, Show)

type Param = (Setting, Word32)
type Payload = Set Param

getSetting :: Get (Either ErrorCode Setting)
getSetting = do
  value <- Get.getWord16be
  return $ Right $ case value of
    0x1 -> HeaderTableSize
    0x2 -> EnablePush
    0x3 -> MaxConcurrentStreams
    0x4 -> InitialWindowSize
    0x5 -> MaxFrameSize
    0x6 -> MaxHeaderListSize

getParam :: Get (Either ErrorCode Param)
getParam = do
  settingEither <- getSetting
  value <- Get.getWord32be
  return $ flip (,) value <$> settingEither

getPayload :: FrameLength -> FrameFlags -> StreamId -> Get (Either ErrorCode Payload)
getPayload length flags sId =
  let (quot, mod) = divMod length 6 in
  let paramCount = fromIntegral quot in
  if mod /= 0 then
    return $ Left FrameSizeError
  else
    let merge xs = fmap (flip Set.insert xs) in
    Monad.foldM merge Set.empty <$> Monad.replicateM paramCount getParam

toString :: String -> Payload -> String
toString prefix = Set.foldr (\(k, v) acc -> prefix ++ show k ++ " = " ++ show v ++ "\n" ++ acc) ""
