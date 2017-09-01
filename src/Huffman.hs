{-# LANGUAGE ViewPatterns, TupleSections #-}
module Huffman
  ( encode
  , encode'
  , decode
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
-- import qualified Data.Binary.Get as Get
-- import Data.Binary.Bits.Get (BitGet)
-- import qualified Data.Binary.Bits.Get as BitGet

import ProjectPrelude

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

table' :: Num a => [(Word8, (Word32,a))]
table' = [
  (  0, (0x00001ff8, 13)),
  (  1, (0x007fffd8, 23)),
  (  2, (0x0fffffe2, 28)),
  (  3, (0x0fffffe3, 28)),
  (  4, (0x0fffffe4, 28)),
  (  5, (0x0fffffe5, 28)),
  (  6, (0x0fffffe6, 28)),
  (  7, (0x0fffffe7, 28)),
  (  8, (0x0fffffe8, 28)),
  (  9, (0x00ffffea, 24)),
  ( 10, (0x3ffffffc, 30)),
  ( 11, (0x0fffffe9, 28)),
  ( 12, (0x0fffffea, 28)),
  ( 13, (0x3ffffffd, 30)),
  ( 14, (0x0fffffeb, 28)),
  ( 15, (0x0fffffec, 28)),
  ( 16, (0x0fffffed, 28)),
  ( 17, (0x0fffffee, 28)),
  ( 18, (0x0fffffef, 28)),
  ( 19, (0x0ffffff0, 28)),
  ( 20, (0x0ffffff1, 28)),
  ( 21, (0x0ffffff2, 28)),
  ( 22, (0x3ffffffe, 30)),
  ( 23, (0x0ffffff3, 28)),
  ( 24, (0x0ffffff4, 28)),
  ( 25, (0x0ffffff5, 28)),
  ( 26, (0x0ffffff6, 28)),
  ( 27, (0x0ffffff7, 28)),
  ( 28, (0x0ffffff8, 28)),
  ( 29, (0x0ffffff9, 28)),
  ( 30, (0x0ffffffa, 28)),
  ( 31, (0x0ffffffb, 28)),
  ( 32, (0x00000014,  6)),
  ( 33, (0x000003f8, 10)),
  ( 34, (0x000003f9, 10)),
  ( 35, (0x00000ffa, 12)),
  ( 36, (0x00001ff9, 13)),
  ( 37, (0x00000015,  6)),
  ( 38, (0x000000f8,  8)),
  ( 39, (0x000007fa, 11)),
  ( 40, (0x000003fa, 10)),
  ( 41, (0x000003fb, 10)),
  ( 42, (0x000000f9,  8)),
  ( 43, (0x000007fb, 11)),
  ( 44, (0x000000fa,  8)),
  ( 45, (0x00000016,  6)),
  ( 46, (0x00000017,  6)),
  ( 47, (0x00000018,  6)),
  ( 48, (0x00000000,  5)),
  ( 49, (0x00000001,  5)),
  ( 50, (0x00000002,  5)),
  ( 51, (0x00000019,  6)),
  ( 52, (0x0000001a,  6)),
  ( 53, (0x0000001b,  6)),
  ( 54, (0x0000001c,  6)),
  ( 55, (0x0000001d,  6)),
  ( 56, (0x0000001e,  6)),
  ( 57, (0x0000001f,  6)),
  ( 58, (0x0000005c,  7)),
  ( 59, (0x000000fb,  8)),
  ( 60, (0x00007ffc, 15)),
  ( 61, (0x00000020,  6)),
  ( 62, (0x00000ffb, 12)),
  ( 63, (0x000003fc, 10)),
  ( 64, (0x00001ffa, 13)),
  ( 65, (0x00000021,  6)),
  ( 66, (0x0000005d,  7)),
  ( 67, (0x0000005e,  7)),
  ( 68, (0x0000005f,  7)),
  ( 69, (0x00000060,  7)),
  ( 70, (0x00000061,  7)),
  ( 71, (0x00000062,  7)),
  ( 72, (0x00000063,  7)),
  ( 73, (0x00000064,  7)),
  ( 74, (0x00000065,  7)),
  ( 75, (0x00000066,  7)),
  ( 76, (0x00000067,  7)),
  ( 77, (0x00000068,  7)),
  ( 78, (0x00000069,  7)),
  ( 79, (0x0000006a,  7)),
  ( 80, (0x0000006b,  7)),
  ( 81, (0x0000006c,  7)),
  ( 82, (0x0000006d,  7)),
  ( 83, (0x0000006e,  7)),
  ( 84, (0x0000006f,  7)),
  ( 85, (0x00000070,  7)),
  ( 86, (0x00000071,  7)),
  ( 87, (0x00000072,  7)),
  ( 88, (0x000000fc,  8)),
  ( 89, (0x00000073,  7)),
  ( 90, (0x000000fd,  8)),
  ( 91, (0x00001ffb, 13)),
  ( 92, (0x0007fff0, 19)),
  ( 93, (0x00001ffc, 13)),
  ( 94, (0x00003ffc, 14)),
  ( 95, (0x00000022,  6)),
  ( 96, (0x00007ffd, 15)),
  ( 97, (0x00000003,  5)),
  ( 98, (0x00000023,  6)),
  ( 99, (0x00000004,  5)),
  (100, (0x00000024,  6)),
  (101, (0x00000005,  5)),
  (102, (0x00000025,  6)),
  (103, (0x00000026,  6)),
  (104, (0x00000027,  6)),
  (105, (0x00000006,  5)),
  (106, (0x00000074,  7)),
  (107, (0x00000075,  7)),
  (108, (0x00000028,  6)),
  (109, (0x00000029,  6)),
  (110, (0x0000002a,  6)),
  (111, (0x00000007,  5)),
  (112, (0x0000002b,  6)),
  (113, (0x00000076,  7)),
  (114, (0x0000002c,  6)),
  (115, (0x00000008,  5)),
  (116, (0x00000009,  5)),
  (117, (0x0000002d,  6)),
  (118, (0x00000077,  7)),
  (119, (0x00000078,  7)),
  (120, (0x00000079,  7)),
  (121, (0x0000007a,  7)),
  (122, (0x0000007b,  7)),
  (123, (0x00007ffe, 15)),
  (124, (0x000007fc, 11)),
  (125, (0x00003ffd, 14)),
  (126, (0x00001ffd, 13)),
  (127, (0x0ffffffc, 28)),
  (128, (0x000fffe6, 20)),
  (129, (0x003fffd2, 22)),
  (130, (0x000fffe7, 20)),
  (131, (0x000fffe8, 20)),
  (132, (0x003fffd3, 22)),
  (133, (0x003fffd4, 22)),
  (134, (0x003fffd5, 22)),
  (135, (0x007fffd9, 23)),
  (136, (0x003fffd6, 22)),
  (137, (0x007fffda, 23)),
  (138, (0x007fffdb, 23)),
  (139, (0x007fffdc, 23)),
  (140, (0x007fffdd, 23)),
  (141, (0x007fffde, 23)),
  (142, (0x00ffffeb, 24)),
  (143, (0x007fffdf, 23)),
  (144, (0x00ffffec, 24)),
  (145, (0x00ffffed, 24)),
  (146, (0x003fffd7, 22)),
  (147, (0x007fffe0, 23)),
  (148, (0x00ffffee, 24)),
  (149, (0x007fffe1, 23)),
  (150, (0x007fffe2, 23)),
  (151, (0x007fffe3, 23)),
  (152, (0x007fffe4, 23)),
  (153, (0x001fffdc, 21)),
  (154, (0x003fffd8, 22)),
  (155, (0x007fffe5, 23)),
  (156, (0x003fffd9, 22)),
  (157, (0x007fffe6, 23)),
  (158, (0x007fffe7, 23)),
  (159, (0x00ffffef, 24)),
  (160, (0x003fffda, 22)),
  (161, (0x001fffdd, 21)),
  (162, (0x000fffe9, 20)),
  (163, (0x003fffdb, 22)),
  (164, (0x003fffdc, 22)),
  (165, (0x007fffe8, 23)),
  (166, (0x007fffe9, 23)),
  (167, (0x001fffde, 21)),
  (168, (0x007fffea, 23)),
  (169, (0x003fffdd, 22)),
  (170, (0x003fffde, 22)),
  (171, (0x00fffff0, 24)),
  (172, (0x001fffdf, 21)),
  (173, (0x003fffdf, 22)),
  (174, (0x007fffeb, 23)),
  (175, (0x007fffec, 23)),
  (176, (0x001fffe0, 21)),
  (177, (0x001fffe1, 21)),
  (178, (0x003fffe0, 22)),
  (179, (0x001fffe2, 21)),
  (180, (0x007fffed, 23)),
  (181, (0x003fffe1, 22)),
  (182, (0x007fffee, 23)),
  (183, (0x007fffef, 23)),
  (184, (0x000fffea, 20)),
  (185, (0x003fffe2, 22)),
  (186, (0x003fffe3, 22)),
  (187, (0x003fffe4, 22)),
  (188, (0x007ffff0, 23)),
  (189, (0x003fffe5, 22)),
  (190, (0x003fffe6, 22)),
  (191, (0x007ffff1, 23)),
  (192, (0x03ffffe0, 26)),
  (193, (0x03ffffe1, 26)),
  (194, (0x000fffeb, 20)),
  (195, (0x0007fff1, 19)),
  (196, (0x003fffe7, 22)),
  (197, (0x007ffff2, 23)),
  (198, (0x003fffe8, 22)),
  (199, (0x01ffffec, 25)),
  (200, (0x03ffffe2, 26)),
  (201, (0x03ffffe3, 26)),
  (202, (0x03ffffe4, 26)),
  (203, (0x07ffffde, 27)),
  (204, (0x07ffffdf, 27)),
  (205, (0x03ffffe5, 26)),
  (206, (0x00fffff1, 24)),
  (207, (0x01ffffed, 25)),
  (208, (0x0007fff2, 19)),
  (209, (0x001fffe3, 21)),
  (210, (0x03ffffe6, 26)),
  (211, (0x07ffffe0, 27)),
  (212, (0x07ffffe1, 27)),
  (213, (0x03ffffe7, 26)),
  (214, (0x07ffffe2, 27)),
  (215, (0x00fffff2, 24)),
  (216, (0x001fffe4, 21)),
  (217, (0x001fffe5, 21)),
  (218, (0x03ffffe8, 26)),
  (219, (0x03ffffe9, 26)),
  (220, (0x0ffffffd, 28)),
  (221, (0x07ffffe3, 27)),
  (222, (0x07ffffe4, 27)),
  (223, (0x07ffffe5, 27)),
  (224, (0x000fffec, 20)),
  (225, (0x00fffff3, 24)),
  (226, (0x000fffed, 20)),
  (227, (0x001fffe6, 21)),
  (228, (0x003fffe9, 22)),
  (229, (0x001fffe7, 21)),
  (230, (0x001fffe8, 21)),
  (231, (0x007ffff3, 23)),
  (232, (0x003fffea, 22)),
  (233, (0x003fffeb, 22)),
  (234, (0x01ffffee, 25)),
  (235, (0x01ffffef, 25)),
  (236, (0x00fffff4, 24)),
  (237, (0x00fffff5, 24)),
  (238, (0x03ffffea, 26)),
  (239, (0x007ffff4, 23)),
  (240, (0x03ffffeb, 26)),
  (241, (0x07ffffe6, 27)),
  (242, (0x03ffffec, 26)),
  (243, (0x03ffffed, 26)),
  (244, (0x07ffffe7, 27)),
  (245, (0x07ffffe8, 27)),
  (246, (0x07ffffe9, 27)),
  (247, (0x07ffffea, 27)),
  (248, (0x07ffffeb, 27)),
  (249, (0x0ffffffe, 28)),
  (250, (0x07ffffec, 27)),
  (251, (0x07ffffed, 27)),
  (252, (0x07ffffee, 27)),
  (253, (0x07ffffef, 27)),
  (254, (0x07fffff0, 27)),
  (255, (0x03ffffee, 26))
  ]

encode' :: ByteString -> ByteString
encode' w = Put.runPut $ BitPut.runBitPut $ do
                  sizes <- mapM encodeChar (ByteString.unpack w)
                  let modSum = sum $ (\s -> mod s 8) <$> sizes
                      rest = modSum `mod` 8
                  BitPut.putWord8 (8 - rest) maxBound

encodeChar :: Word8 -> BitPut Int
encodeChar c = do
       let (bits, size) = array Array.! c
       BitPut.putWord32be size bits
       return size

encode :: ByteString -> ByteString
encode w =
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

decode :: ByteString -> ByteString
decode buf =
  let impl xs i (Leaf w) buf = impl xs i decodingTree (ByteString.snoc buf w)
      impl [] _ _ buf = buf
      impl _ _ EmptyLeaf _ = undefined
      impl (x:xs) i (Node l r) buf =
        let t = if Bits.testBit x i then r else l in
        let (ys, i') = if i == 0 then (xs, 7) else (x:xs, i - 1) in
        impl ys i' t buf in
  impl (ByteString.unpack buf) 7 decodingTree ByteString.empty

{-
dec0 :: Word32 -> BitGet Word8
dec0 0 = return 10
dec0 1 = BitGet.getWord32be 3 >>= dec4
-}


-- table' :: [(Word8, (Word32,Int))]

mkDecode :: (Monad m) => [(Word8, (Word32,Int))] -> (Word32, Int) -> m ()
mkDecode table pre@(prefix, prefixLen) = do
          let prefixTable = filter ((prefixMatch pre) . snd) table
              minPrefix = minimum $ (snd . snd) <$> prefixTable
              newLen = minPrefix - prefixLen
              maxPiece = (2 ^ newLen) -1
              pieceList = [0..maxPiece]
              newPrefixes' = ((Bits.shiftL prefix newLen) Bits..&.) <$> pieceList
              newPrefixes = (,prefixLen + newLen) <$> newPrefixes'
          if newLen == 0
              then undefined -- match found or no match?
              else undefined -- next case
                 

prefixMatch :: (Word32, Int) -> (Word32, Int) -> Bool
prefixMatch (prefix, prefixLen) (word, wordLen) = 
                  let diffLen = wordLen - prefixLen
                      prefix' = Bits.shiftL prefix diffLen
                      mask  = ((2 ^ wordLen) - 1) Bits..&. (Bits.complement ((2 ^ diffLen) - 1))
                    in prefix' == word Bits..&. mask
