module TestHpack(hunitTests, quickcheckTests) where

import qualified Control.Monad.State.Lazy as State
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as ByteString
import qualified Hpack

import Data.ByteString.Lazy(ByteString)
import Data.Text(Text)
import Test.HUnit
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

prop_headerField hf =
  let buffer = Put.runPut (Hpack.putLiteralWithoutIndexing hf) in
  let getM = State.evalStateT Hpack.getLiteralWithoutIndexing [] in
  let hf' = Get.runGet getM buffer in
  hf == hf'

prop_headerFields hfs =
  let buffer = Put.runPut (Hpack.putHeaderFields hfs) in
  let getM = State.evalStateT Hpack.getHeaderFields [] in
  let hfs' = Get.runGet getM buffer in
  hfs == hfs'

return []

quickcheckTests :: IO Bool
quickcheckTests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 1000 })

hex2ByteString :: ByteString -> ByteString
hex2ByteString = ByteString.fromStrict . fst . Base16.decode . ByteString.toStrict

stringTests =
  let parseString = Get.runGet Hpack.getStringLiteral . hex2ByteString in
  let test (result, hex) = TestCase (assertEqual "" result (parseString hex)) in
  TestList $ fmap test [
    ("custom-key", "0a637573746f6d2d6b6579"),
    ("custom-header", "0d637573746f6d2d686561646572"),
    ("/sample/path", "0c2f73616d706c652f70617468") 
  ]

headerTests =
  let parseFields dynTable =
        Get.runGet (State.runStateT Hpack.getHeaderFields dynTable) . hex2ByteString in
  let test (hex, dynTablePre, result, dynTablePost) = TestLabel (show hex) $ TestCase $ do
        let (fields, st) = parseFields dynTablePre hex
        assertEqual "Fields" result fields
        assertEqual "Dynamic table" dynTablePost st in
  TestList $ fmap test [
    ("400a637573746f6d2d6b65790d637573746f6d2d686561646572",
      [],
      [("custom-key", "custom-header")],
      [("custom-key", "custom-header")]),

    ("040c2f73616d706c652f70617468",
      [],
      [(":path", "/sample/path")],
      []),

    ("100870617373776f726406736563726574",
      [],
      [("password", "secret")],
      []),

    ("82",
      [],
      [(":method", "GET")],
      []),

    ("828684410f7777772e6578616d706c652e636f6d",
      [],
      [(":method",    "GET"),
       (":scheme",    "http"),
       (":path",      "/"),
       (":authority", "www.example.com")
      ],
      [(":authority", "www.example.com")]),

    ("828684be58086e6f2d6361636865",
      [(":authority", "www.example.com")],
      [(":method",        "GET"),
       (":scheme",        "http"),
       (":path",          "/"),
       (":authority",     "www.example.com"),
       ("cache-control", "no-cache")
      ],
      [("cache-control", "no-cache"),
       (":authority", "www.example.com")
      ])
  ]

hunitTests = test [stringTests, headerTests]
