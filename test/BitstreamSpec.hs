{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, BinaryLiterals, FlexibleInstances, RecursiveDo #-}

module BitstreamSpec where

import Prelude hiding (words)

import Test.Tasty.Hspec

import Data.Bitstream
import Data.ToBits

import Data.ContBuff

import Data.Bits (zeroBits, setBit)
import Data.String (IsString(..))
import Data.Word (Word8)

import Data.BitCode.LLVM
import Data.BitCode.LLVM.ToBitCode (ToNBitCode(..))
import Data.BitCode.LLVM.Codes.Identification (Epoch(..))

-- | Helper function for the IsString instances
bit :: Int -> Char -> Word8 -> Word8
bit n '1' = flip setBit n
bit _ '0' = id
bit _  b  = error $ "bit must be 0 or 1; " ++ b:" given."  

isBinary :: String -> Bool
isBinary = all (flip elem ['0','1'])

instance IsString Buff where
  fromString s | length s > 8       = error $ "cannot create buffer from " ++ s ++ "; more than eight bits"
               | not . isBinary $ s = error $ "cannot create buffer from " ++ s ++ "; elements must be 0 or 1"
               | otherwise          = Buff (length s, foldl f zeroBits (indexed s))
    where indexed :: [a] -> [(Int, a)]
          indexed = zip [0..]
          f :: Word8 -> (Int, Char) -> Word8
          f w (n, '1') = setBit w n
          f w _        = w


instance IsString Word8 where
  fromString s | length s /= 8      = error $ "cannot create Word8 from " ++ s ++ "; must be 8 bits"
               | not . isBinary $ s = error $ "cannot create Word8 from " ++ s ++ "; elements must be 0 or 1"
               | otherwise          = let (b0:b1:b2:b3:b4:b5:b6:b7:[]) = s
                                      in bit 0 b0 . bit 1 b1 . bit 2 b2 . bit 3 b3
                                         . bit 4 b4 . bit 5 b5 . bit 6 b6 . bit 7 b7 $ zeroBits

instance {-# OVERLAPS #-} IsString [Word8] where
  fromString = fromString' . filter (/= ' ')
    where fromString' :: String -> [Word8]
          fromString' s | (length s `mod` 8) /= 0     = error $ "cannot create [Word8] from " ++ s ++ "; must be multiple of 8 bits"
                        | not . isBinary $ s = error $ "cannot create [Word8] from " ++ s ++ "; elements must be 0 or 1"
                        | otherwise          = go s
        
          go :: String -> [Word8]
          go [] = []
          go (b0:b1:b2:b3:b4:b5:b6:b7:rest) = let word = bit 0 b0 . bit 1 b1 . bit 2 b2 . bit 3 b3
                                                         . bit 4 b4 . bit 5 b5 . bit 6 b6 . bit 7 b7 $ zeroBits
                                              in word : go rest
          go s = error $ "cannot creates [Word8] from " ++ s ++ "; must be multiple of 8 chars."

instance IsString (Stream [] a) where
  fromString = fromString' . filter (/= ' ')
    where fromString' :: String -> Stream [] a
          fromString' s | not . isBinary $ s = error $ "cannot create List Stream from " ++ s ++ "; elements must be 0 or 1"
                        | otherwise          = let (ws, buff) = go s in S ws buff (length s)
          go :: String -> ([Word8], Buff)
          go s = let l = 8 * (length s `div` 8) in
                   (words $ take l s, b $ drop l s)

-- instance IsString TStream where
--   fromString = fromString' . filter (/= ' ')
--     where fromString' :: String -> TStream
--           fromString' s | not . isBinary $ s = error $ "cannot create TStream from " ++ s ++ "; elements must be 0 or 1"
--                         | otherwise          = let (ws,b) = go s in TS (ws, b, (length s) `mod` buffSize)
--           go :: String -> ([Word8],(Int, Word8)) -> ([Word8],(Int,Word8))
--           go [] = (id,0)
--           go xs = case splitAt buffSize xs of
--             (h,[]) -> \x -> x `mappend` (TS ()) 
--             (h, t) -> let (bs, b) = go t in ((word h:) . bs, b)
--             where word :: String -> Word8
--                   word x = foldr (uncurry bit) zeroBits (zip [0..] x) 

instance IsString (BList Word8) where
  fromString = fromString' . filter (/= ' ')
    where fromString' :: String -> BList Word8
          fromString' s | not . isBinary $ s = error $ "cannot create BList from " ++ s ++ "; elements must be 0 or 1"
                        | otherwise          = BL $ go s
          go :: String -> [Buffer Word8]
          go = map toBuffer . chunksOf 8
          toBuffer :: String -> Buffer Word8
          toBuffer s = case length s of
            l | l < 8     -> Partial l (p s)
              | otherwise -> Full (w s)

-- type helper.
b :: String -> Buff
b = fromString

w :: String -> Word8
w = fromString

p :: String -> Word8
p = fromString . take 8 . (++ "00000000") 

words :: String -> [Word8]
words = fromString

ls :: String -> Stream [] Word8
ls = fromString

bl8 :: String -> BList Word8
bl8 = fromString
-- ts :: String -> TStream
-- ts = fromString

-- runTS :: TStream -> ([Word8], Int)
-- runTS (TS (c, b, l)) | l > 0     = (c [b], l)
--                      | otherwise = (c [], l)

-- ts' :: String -> ([Word8], Int)
-- ts' = runTS . ts

-- * Specifications

spec_helper :: Spec
spec_helper = do
  describe "Buff" $ do
    it "has an IsString instance" $ do
      "0" `shouldBe` (Buff (1,0b00000000))
      "1" `shouldBe` (Buff (1,0b00000001))
      "11" `shouldBe` (Buff (2,0b00000011))
      "10100101" `shouldBe` (Buff (8, 0b10100101))
      "10101010" `shouldBe` (Buff (8, 0b01010101))

  describe "Word8" $ do
    it "has an IsString instance" $ do
      w "00000000" `shouldBe` 0
      w "10000000" `shouldBe` 1
      w "11000000" `shouldBe` 3
      w "00000001" `shouldBe` 128

  describe "List Stream" $ do
    it "has an IsString instance" $ do
      ls "" `shouldBe` (S [] nullBuff 0) 
      ls "1" `shouldBe` (S [] (Buff (1,0b00000001)) 1)
      ls "10101010 101" `shouldBe` (S [0b01010101] (Buff (3, 0b00000101)) 11)

  -- describe "TStream" $ do
  --   it "has an IsString instance" $ do
  --     ts' "" `shouldBe` ([],0)
  --     ts' "0" `shouldBe` ([0],1)
  --     ts' "1" `shouldBe` ([0b00000001],1)
  --     ts' "10000000 1" `shouldBe` ([0b00000001,1], 1)

  describe "BList" $ do
    it "has an IsString instance" $ do
      bl8 "" `shouldBe` (BL [])
      bl8 "1" `shouldBe` (BL [Partial 1 0b00000001])
      bl8 "11" `shouldBe` (BL [Partial 2 0b00000011])
      bl8 "10101010" `shouldBe` (BL [Full 0b01010101])
      bl8 "10101010 1010" `shouldBe` (BL [ Full 0b01010101
                                         , Partial 4 0b00000101])

spec_blist :: Spec
spec_blist = do
  describe "join" $ do
    it "should join word8" $ do
      join 0 (w "10000000") (w "10000000")
        `shouldBe` (w "10000000", 0)
      join 1 (w "10000000") (w "10000000")
        `shouldBe` (w "11000000", 0)
      join 2 (w "10000000") (w "10000000")
        `shouldBe` (w "10100000", 0)
      join 1 (w "10000000") (w "01101001")
        `shouldBe` (w "10110100", w "10000000")
      join 8 (w "00000001") (w "10000000")
        `shouldBe` (w "00000001", w "10000000")

  describe "BList Word8" $ do
    it "should be a monoid" $ do
      mempty `shouldBe` (BL [] :: BList Word8)
      bl8 "1" `mappend` (bl8 "1")
        `shouldBe` (bl8 "11")

      bl8 "10" `mappend` bl8 "1010"
        `shouldBe` bl8 "101010"

      bl8 "101010" `mappend` (bl8 "111000")
        `shouldBe` bl8 "101010 111000"

      bl8 "101" `mappend` (bl8 "10101010 101")
        `shouldBe` bl8 "10110101 010101"

      bl8 "00000000 10101" `mappend` bl8 "11111111 001"
        `shouldBe` bl8 "00000000 10101111 11111001"

spec_buff :: Spec
spec_buff = do
  describe "Buff" $ do
    it "should add" $ do
      nullBuff `addBuff` nullBuff
        `shouldBe` (Nothing,nullBuff)
      nullBuff `addBuff` (Buff (4,0b00000101))
        `shouldBe` (Nothing, (Buff (4,0b00000101)))
      (Buff (1,0b00000001)) `addBuff` (Buff (1,0b00000001))
        `shouldBe` (Nothing, (Buff (2,0b00000011)))
      (Buff (4,0b00000101)) `addBuff` (Buff (4,0b00000101))
        `shouldBe` (Just 0b01010101, nullBuff)
      (Buff (6,0b00010101)) `addBuff` (Buff (4,0b00001010))
        `shouldBe` (Just 0b10010101, (Buff (2,0b00000010)))
      (Buff (6,0b00010101)) `addBuff` (Buff (7,0b01010101))
        `shouldBe` (Just 0b01010101, (Buff (5,0b00010101)))

spec_stream :: Spec
spec_stream = do
  describe "Stream" $ do
    it "should be a monoid" $ do
      (S []  nullBuff 0 `mappend` S []  (Buff (4,0b10100000)) 4)
        `shouldBe` (S []    (Buff (4,0b10100000)) 4)  
      (S [1] nullBuff 8 `mappend` S [2] (Buff (4,0b10100000)) 12)
        `shouldBe` (S [1,2] (Buff (4,0b10100000)) 20)
      (S []  (Buff (4,0b00001010)) 4 `mappend` S [] (Buff (4,0b00000101)) 4)
        `shouldBe` (S [0b01011010] nullBuff 8)

      --   101 + 10101010 101
      -- = 10110101 010101
      (S [] (Buff (3,0b00000101)) 3) `mappend` (S [0b01010101] (Buff (3,0b00000101)) 11)
        `shouldBe` (S [0b10101101] (Buff (6, 0b00101010)) 14)

      ls "101" `mappend` (ls "10101010 101") `shouldBe` (ls "10110101 010101")

      --   01010101 101 + 10101010 11001100 000111
      -- = 01010101 10110101 01011001 10000011 1

      ls "01010101 101" `mappend` (ls "10101010 11001100 000111")
        `shouldBe` (ls "01010101 10110101 01011001 10000011 1")
      
      (S [0b10101010] (Buff (3,0b00000101)) 11) `mappend` (S [0b01010101, 0b00110011] (Buff (6,0b00111000)) 22)
        `shouldBe` (S [0b10101010,0b10101101,0b10011010,0b11000001] (Buff (1,0b00000001)) 33)


-- spec_tstream :: Spec
-- spec_tstream = do
--   describe "TStream" $ do
--     it "should be a monoid" $ do
--       runTS (ts "" `mappend` ts "") `shouldBe` ([],0)
--       runTS (ts "" `mappend` ts "1010") `shouldBe` ([0b00000101], 4)
--       runTS (ts "1010" `mappend` ts "") `shouldBe` ([0b00000101], 4)
--       runTS (ts "1010" `mappend` ts "0011") `shouldBe` ([0b11000101], 0)

--       ts "101" `mappend` ts "10101010 101"
--         `shouldBe` ts "10110101 010101"

spec_bitstream :: Spec
spec_bitstream = do
  describe "Bitstream" $ do
    it "should track location" $ do
      evalBitstream 0 (loc) `shouldBe` 0
      evalBitstream 0 (emitBit True >> loc) `shouldBe` 1
      evalBitstream 0 (emitBit True >> emitBit False >> loc) `shouldBe` 2
      evalBitstream 0 (emitBit True >> alignWord8 >> loc) `shouldBe` 8

    it "should produce word aligned results" $ do
      execBitstream 0 (pure ()) `shouldBe` []
      execBitstream 0 (emitBit False) `shouldBe` [0b00000000]
      execBitstream 0 (emitBit True) `shouldBe`  [0b00000001]
      execBitstream 0 (emitBit True >> emitBit False >> emitBit True) `shouldBe` [0b00000101]

    it "should produce the proper darwin header" $ do
      execBitstream 0 (withHeader True (pure ())) `shouldBe`
        [ 0xde, 0xc0, 0x17, 0x0b -- 0x0b17c0de header
        , 0x00, 0x00, 0x00, 0x00 -- version: 0
        , 0x14, 0x00, 0x00, 0x00 -- offset: 20
        , 0x04, 0x00, 0x00, 0x00 -- body length: 4 (llvmheader)
        , 0x07, 0x00, 0x00, 0x01 -- cpu type: ABI64 | X86
        , 0x42, 0x43, 0xc0, 0xde -- LLVM header. "BC" 0x0de
        ]
    it "should be able to emit a fixed number of bits" $ do
      execBitstream 0 (emitFixed 6 0) `shouldBe` [0x00]
      execBitstream 0 (emitFixed 6 1) `shouldBe` (words "10000000")
      execBitstream 0 (emitFixed 6 2) `shouldBe` (words "01000000")
      execBitstream 0 (emitFixed 6 1 >> emitFixed 6 2)
        `shouldBe` (words "100000 010000 0000")
      
    describe "should be able to emit a variable number of bits" $ do
      it "emitVBR 3 9" $ execBitstream 0 (emitVBR 3 1) `shouldBe` (words "10000000")
      it "emitVBR 3 2" $ execBitstream 0 (emitVBR 3 2) `shouldBe` (words "01000000")
      it "emitVBR 3 3" $ execBitstream 0 (emitVBR 3 3) `shouldBe` (words "11000000")
      it "emitVBR 3 4" $ execBitstream 0 (emitVBR 3 4) `shouldBe` (words "00110000")
      it "emitVBR 3 5" $ execBitstream 0 (emitVBR 3 5) `shouldBe` (words "10110000")
      it "emitVBR 3 9" $ execBitstream 0 (emitVBR 3 9) `shouldBe` (words "10101000")
      it "emitVBR 4 0" $ execBitstream 0 (emitVBR 4  0) `shouldBe` (words "00000000")
      it "emitVBR 4 1" $ execBitstream 0 (emitVBR 4  1) `shouldBe` (words "10000000")
      it "emitVBR 4 2" $ execBitstream 0 (emitVBR 4  2) `shouldBe` (words "01000000")
      it "emitVBR 4 4" $ execBitstream 0 (emitVBR 4  4) `shouldBe` (words "00100000")
      it "emitVBR 4 8" $ execBitstream 0 (emitVBR 4  8) `shouldBe` (words "00011000")
      it "emitVBR 4 16" $ execBitstream 0 (emitVBR 4 16) `shouldBe` (words "00010100")
      it "emitVBR 4 32" $ execBitstream 0 (emitVBR 4 32) `shouldBe` (words "00010010")
      it "emitVBR 4 64" $ execBitstream 0 (emitVBR 4 64) `shouldBe` (words "00010001 10000000")

    it "should be able to emit char6 encoded data" $ do
      execBitstream 0 (mapM emitChar6 ("abcd" :: String))
        `shouldBe` (words "000000 100000 010000 110000")

    it "handle withOffset" $ do
      True `shouldBe` True
      let action :: Bitstream ()
          action = mdo
            emitWord32 n
            n <- withOffset 0 $ do
              emitWord32 0
              emitWord32 0xffffffff
              emitWord32 0
              emitWord32 0xff00ff00
              locWords -- should be two now.
            emitWord8 3
            pure ()
      execBitstream 0 action `shouldBe`
        [ 0x04, 0x00, 0x00, 0x00  -- four words
        , 0x00, 0x00, 0x00, 0x00  -- word 1
        , 0xff, 0xff, 0xff, 0xff  -- word 2
        , 0x00, 0x00, 0x00, 0x00  -- word 3
        , 0x00, 0xff, 0x00, 0xff  -- word 4
        , 0x03 ]

    it "should align to word32" $ do
      execBitstream 0 (emitFixed 6 1 >> alignWord32)
        `shouldBe` (words "1000 0000 0000 0000 0000 0000 0000 0000")
      execBitstream 0 (emitFixed 6 1 >> emitFixed 6 1 >> alignWord32)
        `shouldBe` (words "1000 0010 0000 0000 0000 0000 0000 0000")
      execBitstream 0 (emitWord32 0 >> alignWord32)
        `shouldBe` [0x00,0x00,0x00,0x00]

spec_bitcode :: Spec
spec_bitcode = do
  describe "bitcode serializer" $ do
    it "should emit a simple empy block" $ do
      -- emit a block with id 1, and no body.
      let action = emitTopLevel [Block 1 3 []]
          result = execBitstream 0 (action)
      result `shouldBe`
        (words $ "10 10000000 1100"   -- enter block (1), block id (1), len (3)
              ++                  "00 0000 0000 0000 0000" -- align 32 bits
              ++ "1000 0000 0000 0000 0000 0000 0000 0000" -- body contains 1 word
              ++ "000"                -- end block (0)
              ++    "0 0000 0000 0000 0000 0000 0000 0000" -- align 32 bits
        )
 
    it "should serialize an ident block" $ do
      let bc = map denormalize $ toBitCode (Ident "LLVM" Current)
          result = execBitstream 0 (emitTopLevel bc)
      result `shouldBe`
        (words $ "10 10110000 0100" -- block: 13, len: 2           2+8+4 = 14
              ++                  "00 0000 0000 0000 0000"   --    2+4*4 = 18
              ++ "1100 0000 0000 0000 0000 0000 0000 0000"   -- body contains 3 words
              ++ "11 100000 001000" -- unabbrev record: 1, 4 ops   2+6+6 = 14
              ++ "001101010000"     -- vbr:6 ('L')                       = 12
              ++ "001101010000"     -- vbr:6 ('L')                       = 12
              ++ "011011010000"     -- vbr:6 ('V')                       = 12
              ++ "101101010000"     -- vbr:6 ('M')                       = 12
              ++ "11 010000 100000" -- unabbrev record: 2, 1 op    2+6+6 = 14
              ++ "000000"           -- vbr:6 (0)                         =  6
              ++ "00"               -- end block (0)                     =  2
                                    --                               sum = 84 = 2 * 32 + 20
              ++ "0000 0000 0000"   -- align to word boundary.
        )
    it "should serialize an empty module" $ do
      let bc = toBitCode (Just (Ident "LLVM" Current), Module { mVersion = 1
                                                              , mTriple = Nothing
                                                              , mDatalayout = Nothing
                                                              , mValues = []
                                                              , mDecls = []
                                                              , mDefns = []
                                                              , mFns = []
                                                              , mConsts = []
                                                              , mTypes = []})
          bc' = map denormalize bc
          result = execBitstream 0 (emitTopLevel bc')
      result `shouldBe`
        [ 0x35, 0x08, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00
        , 0x07, 0x04, 0x2b, 0xb0, 0x82, 0x2d, 0xb4, 0xc2
        , 0x42, 0x00, 0x00, 0x00, 0x21, 0x14, 0x00, 0x00
        , 0x08, 0x00, 0x00, 0x00, 0x23, 0x08, 0x82, 0x10
        , 0x21, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00
        , 0x07, 0x01, 0x00, 0x00, 0xc1, 0x41, 0x00, 0x00
        , 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        , 0x00, 0x00, 0x00, 0x00
        ]

        
