module Hpack(
  PrefixLength(..),
  getInteger,
  getStringLiteral,
  putInteger,
  putStringLiteral
) where

import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as ByteString

import qualified Huffman

import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.Bits((.|.), (.&.))
import Data.ByteString.Lazy(ByteString)
import Data.Word(Word8, Word64)

-- Use to limit the domain of some functions like putInteger and getInteger
data PrefixLength = One | Two | Three | Four | Five | Six | Seven | Eight
  deriving Show

instance Enum PrefixLength where
  toEnum 1 = One
  toEnum 2 = Two
  toEnum 3 = Three
  toEnum 4 = Four
  toEnum 5 = Five
  toEnum 6 = Six
  toEnum 7 = Seven
  toEnum 8 = Eight
  toEnum _ = undefined

  fromEnum One   = 1
  fromEnum Two   = 2
  fromEnum Three = 3
  fromEnum Four  = 4
  fromEnum Five  = 5
  fromEnum Six   = 6
  fromEnum Seven = 7
  fromEnum Eight = 8

instance Bounded PrefixLength where
  minBound = One
  maxBound = Eight

-- Primitive Type Representations - Integer Representation
-- cf. https://tools.ietf.org/html/rfc7541#section-5.1
getInteger :: PrefixLength -> Get (Word8, Word64)
getInteger pLen =
  let n = fromEnum pLen in
  let m = 2^n - 1 :: Word8 in do
  octet <- Get.getWord8
  let i = octet .&. m
  let prefix = octet .&. Bits.complement m
  if i < m then
    return (prefix, fromIntegral i)
  else
    let impl :: Word64 -> Word64 -> Get (Word8, Word64)
        impl i m = do
          b <- Get.getWord8
          let i' = i + fromIntegral (b .&. 127) * 2^m
          let m' = m + 7
          if Bits.testBit b 7 then
            impl i' m'
          else
            return (prefix, i') in
    impl (fromIntegral i) 0

putInteger :: Word8 -> PrefixLength -> Word64 -> Put
putInteger octet pLen i =
  let n = fromEnum pLen in
  let m = 2^n - 1 in
  if i < m then
    Put.putWord8 (octet .&. Bits.complement (fromIntegral m) .|. fromIntegral i)
  else
    let impl i =
          if i >= 128 then do
            Put.putWord8 (fromIntegral (i `mod` 128) + 128)
            impl (i `div` 128)
          else
            Put.putWord8 (fromIntegral i) in do
    Put.putWord8 (octet .|. fromIntegral m)
    impl (i - m)

-- Primitive Type Representations - String Literal Representation
-- cf. https://tools.ietf.org/html/rfc7541#section-5.2
getStringLiteral :: Get ByteString
getStringLiteral = do
  (prefix, strLen) <- getInteger Seven
  buf <- Get.getLazyByteString (fromIntegral strLen)
  return (if Bits.testBit prefix 7 then Huffman.decode buf else buf)

putStringLiteral :: Bool -> ByteString -> Put
putStringLiteral huffman buf = do
  let (prefix, buf') = if huffman then (128, Huffman.encode buf) else (0, buf)
  let bufLen = fromIntegral (ByteString.length buf')
  putInteger prefix Seven bufLen
  Put.putLazyByteString buf'
