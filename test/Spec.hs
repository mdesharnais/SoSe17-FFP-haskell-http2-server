{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put

import qualified Hpack

import System.Exit(exitFailure, exitSuccess)
import Test.QuickCheck

import Hpack(PrefixLength)

instance Arbitrary PrefixLength where
  arbitrary = arbitraryBoundedEnum

prop_integer octet prefixLength i =
  let buffer = Put.runPut (Hpack.putInteger octet prefixLength i) in
  let i' = Get.runGet (Hpack.getInteger prefixLength) buffer in
  i == i'

return []

main :: IO ()
main = do
  putStrLn ""
  success <- $quickCheckAll
  if success then
    exitSuccess
  else
    exitFailure
