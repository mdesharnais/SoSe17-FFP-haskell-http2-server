import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put

import qualified Hpack
import qualified TestHuffman

import System.Exit(exitFailure, exitSuccess)
import Test.QuickCheck
import Test.QuickCheck.Instances

import Hpack(PrefixLength)

instance Arbitrary PrefixLength where
  arbitrary = arbitraryBoundedEnum

prop_integer octet prefixLength i =
  let buffer = Put.runPut (Hpack.putInteger octet prefixLength i) in
  let (_, i') = Get.runGet (Hpack.getInteger prefixLength) buffer in
  i == i'

prop_stringLiteralWithoutHuffman str =
  let buffer = Put.runPut (Hpack.putStringLiteral False str) in
  let str' = Get.runGet Hpack.getStringLiteral buffer in
  str == str'

prop_stringLiteralWithHuffman str =
  let buffer = Put.runPut (Hpack.putStringLiteral True str) in
  let str' = Get.runGet Hpack.getStringLiteral buffer in
  str == str'

return []

tests :: IO Bool
tests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 1000 })

main :: IO ()
main = do
  putStrLn ""
  success <- (&&) <$> tests <*> TestHuffman.tests
  if success then
    exitSuccess
  else
    exitFailure
