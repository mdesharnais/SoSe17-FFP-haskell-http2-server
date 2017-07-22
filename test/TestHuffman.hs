module TestHuffman where

import qualified Data.ByteString.Lazy as ByteString

import Test.QuickCheck
import Test.QuickCheck.Instances

import qualified Huffman

prop_convertWord32ToBits w n =
  1 <= n && n <= 32 ==> length (Huffman.convertWord32ToBits w n) == fromIntegral n

prop_decodeEncode buf =
  ByteString.length buf <= 100 ==> Huffman.decode (Huffman.encode buf) == buf

return []

tests :: IO Bool
tests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 1000 })
