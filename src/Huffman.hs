{-# LANGUAGE ViewPatterns, TupleSections, TemplateHaskell #-}
module Huffman
  ( encode
  , encode'
  , decode
  , decode'
  , convertWord32ToBits
  , decodingTree
  ) where

import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import Data.ByteString.Lazy(ByteString)
import Data.Map.Strict(Map, (!))

import Data.Array (Array)
import qualified Data.Array as Array
import Data.Binary.Bits.Put (BitPut)
import qualified Data.Binary.Bits.Put as BitPut
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import Data.Binary.Bits.Get (BitGet)
import qualified Data.Binary.Bits.Get as BitGet

import ProjectPrelude
import HuffmanTH

type Bits = [Bool]

convertWord32ToBits :: Word32 -> Word8 -> Bits
convertWord32ToBits w !1 = [Bits.testBit w 0]
convertWord32ToBits w !n
  | 1 <= n && n <= 32 =
      Bits.testBit w (fromIntegral (n - 1)) : convertWord32ToBits w (n - 1)
  | otherwise = undefined

padding :: Bits
padding = [True, True, True, True, True, True, True]

table :: Map Word8 Bits
table = uncurry convertWord32ToBits <$> Map.fromList table'

array :: Array Word8 (Word32,Int)
array = Array.array (0,255) table'


encode :: ByteString -> ByteString
encode w = Put.runPut $ BitPut.runBitPut $ do
                  sizes <- mapM encodeChar (ByteString.unpack w)
                  let modSum = sum $ (\s -> mod s 8) <$> sizes
                      rest = modSum `mod` 8
                  BitPut.putWord8 (8 - rest) maxBound

encodeChar :: Word8 -> BitPut Int
encodeChar c = do
       let (bits, size) = array Array.! c
       BitPut.putWord32be size bits
       return size

encode' :: ByteString -> ByteString
encode' w =
  let impl :: ByteString -> Bits -> ByteString
      impl buffer (b7:b6:b5:b4:b3:b2:b1:b0:bs) =
        let x = 0 :: Word8 in
        let !x7 = if b7 then Bits.setBit x 7 else x in
        let !x6 = if b6 then Bits.setBit x7 6 else x7 in
        let !x5 = if b5 then Bits.setBit x6 5 else x6 in
        let !x4 = if b4 then Bits.setBit x5 4 else x5 in
        let !x3 = if b3 then Bits.setBit x4 3 else x4 in
        let !x2 = if b2 then Bits.setBit x3 2 else x3 in
        let !x1 = if b1 then Bits.setBit x2 1 else x2 in
        let !x0 = if b0 then Bits.setBit x1 0 else x1 in
        impl (ByteString.snoc buffer x0) bs
      impl buffer _ = buffer in
  impl ByteString.empty $ ByteString.foldr (\w acc -> table ! w ++ acc) padding w

data Tree = EmptyLeaf | Leaf Word8 | Node Tree Tree deriving Show

decodingTree :: Tree
decodingTree =
  let goesLeft (_, []) = undefined
      goesLeft (_, b:_) = not b in
  let impl :: [(Word8, Bits)] -> Maybe Tree
      impl [] = Nothing
      impl [(w, [])] = Just (Leaf w)
      impl [(w, [True])] = Just (Node EmptyLeaf (Leaf w))
      impl [(w, [False])] = Just (Node (Leaf w) EmptyLeaf)
      impl [(_, _)] = Nothing
      impl (List.partition goesLeft -> (ls, rs)) =
        let f (_, []) = undefined
            f (w, _:xs) = (w, xs) in
        Node <$> impl (map f ls) <*> impl (map f rs) in
  Maybe.fromJust (impl (Map.toList table))

decode' :: ByteString -> ByteString
decode' buf =
  let impl xs i (Leaf w) buf = impl xs i decodingTree (ByteString.snoc buf w)
      impl [] _ _ buf = buf
      impl _ _ EmptyLeaf _ = undefined
      impl (x:xs) i (Node l r) buf =
        let t = if Bits.testBit x i then r else l in
        let (ys, i') = if i == 0 then (xs, 7) else (x:xs, i - 1) in
        impl ys i' t buf in
  impl (ByteString.unpack buf) 7 decodingTree ByteString.empty

decodeChar :: BitGet HuffmanChar
decodeChar = $(mkDecode maybeTable (0,0))

decodeBitGet :: BitGet [Word8]
decodeBitGet = do
              w <- decodeChar
              case w of
                  EndHuffman -> return []
                  HuffmanChar c -> (c :) <$> decodeBitGet
                  HuffmanError -> undefined

decode :: ByteString -> ByteString
decode bs = let bs' = ByteString.append bs (ByteString.pack [0xff, 0xff, 0xff, 0xff])
                bitget = BitGet.runBitGet decodeBitGet
             in ByteString.pack $ Get.runGet bitget bs'
