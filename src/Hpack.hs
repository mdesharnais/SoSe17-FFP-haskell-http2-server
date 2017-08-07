module Hpack(
  HeaderField,
  HeaderName,
  HeaderValue,
  Headers,
  PrefixLength(..),
  getHeaderFields,
  getInteger,
  getLiteralWithoutIndexing,
  getStringLiteral,
  putInteger,
  putHeaderFields,
  putLiteralWithoutIndexing,
  putStringLiteral
) where

import qualified Control.Monad.State.Lazy as State
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.Encoding as Encoding

import qualified Huffman

import Control.Monad.State.Lazy(StateT)
import Control.Monad.Trans.Class(lift)
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.Bits((.|.), (.&.))
import Data.ByteString.Lazy(ByteString)
import Data.Text(Text)
import Data.Word(Word8, Word32)

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
getInteger :: PrefixLength -> Get (Word8, Word32)
getInteger pLen =
  let n = fromEnum pLen in
  let m = 2^n - 1 :: Word8 in do
  octet <- Get.getWord8
  let i = octet .&. m
  let prefix = octet .&. Bits.complement m
  if i < m then
    return (prefix, fromIntegral i)
  else
    let impl :: Word32 -> Word32 -> Get (Word8, Word32)
        impl i m = do
          b <- Get.getWord8
          let i' = i + fromIntegral (b .&. 127) * 2^m
          let m' = m + 7
          if Bits.testBit b 7 then
            impl i' m'
          else
            return (prefix, i') in
    impl (fromIntegral i) 0

putInteger :: Word8 -> PrefixLength -> Word32 -> Put
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

getTextLiteral :: Get Text
getTextLiteral = Encoding.decodeUtf8 . ByteString.toStrict <$> getStringLiteral

putTextLiteral :: Bool -> Text -> Put
putTextLiteral b = putStringLiteral b . ByteString.fromStrict . Encoding.encodeUtf8
-- Binary Format
-- cf. https://tools.ietf.org/html/rfc7541#section-6

type HeaderName = Text
type HeaderValue = Text
type HeaderField = (HeaderName, HeaderValue)
type Headers = [HeaderField]

staticTable :: Headers
staticTable = [
  ("",                            ""),
  (":authority",                  ""),
  (":method",                     "GET"),
  (":method",                     "POST"),
  (":path",                       "/"),
  (":path",                       "/index.html"),
  (":scheme",                     "http"),
  (":scheme",                     "https"),
  (":status",                     "200"),
  (":status",                     "204"),
  (":status",                     "206"),
  (":status",                     "304"),
  (":status",                     "400"),
  (":status",                     "404"),
  (":status",                     "500"),
  ("accept-charset",              ""),
  ("accept-encoding",             "gzip, deflate "),
  ("accept-language",             ""),
  ("accept-ranges",               ""),
  ("accept",                      ""),
  ("access-control-allow-origin", ""),
  ("age",                         ""),
  ("allow",                       ""),
  ("authorization",               ""),
  ("cache-control",               ""),
  ("content-disposition",         ""),
  ("content-encoding",            ""),
  ("content-language",            ""),
  ("content-length",              ""),
  ("content-location",            ""),
  ("content-range",               ""),
  ("content-type",                ""),
  ("cookie",                      ""),
  ("date",                        ""),
  ("etag",                        ""),
  ("expect",                      ""),
  ("expires",                     ""),
  ("from",                        ""),
  ("host",                        ""),
  ("if-match",                    ""),
  ("if-modified-since",           ""),
  ("if-none-match",               ""),
  ("if-range",                    ""),
  ("if-unmodified-since",         ""),
  ("last-modified",               ""),
  ("link",                        ""),
  ("location",                    ""),
  ("max-forwards",                ""),
  ("proxy-authenticate",          ""),
  ("proxy-authorization",         ""),
  ("range",                       ""),
  ("referer",                     ""),
  ("refresh",                     ""),
  ("retry-after",                 ""),
  ("server",                      ""),
  ("set-cookie",                  ""),
  ("strict-transport-security",   ""),
  ("transfer-encoding",           ""),
  ("user-agent",                  ""),
  ("vary",                        ""),
  ("via",                         ""),
  ("www-authenticate",            "")
  ]

staticTableLength :: Int
staticTableLength = length staticTable

type DynamicTable = Headers

getFromTable :: DynamicTable -> Word32 -> HeaderField
getFromTable dynamicTable index =
  let idx = fromIntegral index in
  if idx < staticTableLength then
    staticTable !! idx
  else
    dynamicTable !! (idx - staticTableLength)

-- Indexed Header Field Representation
-- https://tools.ietf.org/html/rfc7541#section-6.1
getIndexed :: StateT DynamicTable Get HeaderField
getIndexed = do
  (_, idx) <- lift $ getInteger Seven
  dynTbl <- State.get
  return $ getFromTable dynTbl idx 

-- Literal Header Field with Incremental Indexing
-- https://tools.ietf.org/html/rfc7541#section-6.2.1
getLiteralWithIndexing :: StateT DynamicTable Get HeaderField
getLiteralWithIndexing = do
  (_, index) <- lift $ getInteger Six
  let idx = fromIntegral index
  name <-
    if idx == 0 then
      lift getTextLiteral
    else do
      fst . flip getFromTable idx <$> State.get
  value <- lift getTextLiteral
  let header = (name, value)
  State.modify ((:) header)
  return header

-- Literal Header Field without Indexing
-- https://tools.ietf.org/html/rfc7541#section-6.2.2
getLiteralWithoutIndexing :: StateT DynamicTable Get HeaderField
getLiteralWithoutIndexing = do
  (_, index) <- lift $ getInteger Four
  let idx = fromIntegral index
  name <-
    if idx == 0 then
      lift getTextLiteral
    else do
      fst . flip getFromTable idx <$> State.get
  value <- lift getTextLiteral
  return (name, value)

-- Literal Header Field never Index
-- https://tools.ietf.org/html/rfc7541#section-6.2.3
getLiteralNeverIndex :: StateT DynamicTable Get HeaderField
getLiteralNeverIndex = getLiteralWithoutIndexing

getHeaderField :: StateT DynamicTable Get HeaderField
getHeaderField = do
  byte <- lift $ Get.lookAhead Get.getWord8
  if Bits.testBit byte 7 then
    getIndexed
  else if Bits.testBit byte 6 then
    getLiteralWithIndexing
  else if Bits.testBit byte 5 then
    -- Dynamic Table Size Update
    -- https://tools.ietf.org/html/rfc7541#section-6.3
    undefined
  else if Bits.testBit byte 4 then
    getLiteralNeverIndex
  else
    getLiteralWithoutIndexing

getHeaderFields :: StateT DynamicTable Get Headers
getHeaderFields =
  let impl xs = do
        empty <- lift Get.isEmpty
        if empty then
          return $ reverse xs
        else do
          hf <- getHeaderField
          impl (hf : xs)
   in impl []


-- Literal Header Field without Indexing
-- https://tools.ietf.org/html/rfc7541#section-6.2.2
putLiteralWithoutIndexing :: HeaderField -> Put
putLiteralWithoutIndexing (k, v) = do
  putInteger 0x00 Four 0
  putTextLiteral False k
  putTextLiteral False v

putHeaderFields :: Headers -> Put
putHeaderFields = mapM_ putLiteralWithoutIndexing
