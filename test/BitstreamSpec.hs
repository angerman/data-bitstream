{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, BinaryLiterals, FlexibleInstances #-}

module BitstreamSpec where

import Test.Tasty.Hspec

import Data.Bitstream

import Data.Bits (zeroBits, setBit)
import Data.String (IsString(..))
import Data.Word (Word8)


instance IsString Buff where
  fromString s | length s > 8                        = error $ "cannot create buffer from " ++ s ++ "; more than eight bits"
               | not . all (flip elem ['0','1']) $ s = error $ "cannot create buffer from " ++ s ++ "; elements must be 0 or 1"
               | otherwise                           = Buff (length s, foldl f zeroBits (indexed s))
    where indexed :: [a] -> [(Int, a)]
          indexed = zip [0..]
          f :: Word8 -> (Int, Char) -> Word8
          f w (n, '1') = setBit w n
          f w _        = w

b :: String -> Buff
b = fromString

instance IsString Word8 where
  fromString s | length s /= 8                        = error $ "cannot create Word8 from " ++ s ++ "; must be 8 bits"
               | not . all (flip elem ['0', '1']) $ s = error $ "cannot create Word8 from " ++ s ++ "; elements must be 0 or 1"
               | otherwise                            = let (b0:b1:b2:b3:b4:b5:b6:b7:[]) = s
                                                        in bit 0 b0 . bit 1 b1 . bit 2 b2 . bit 3 b3
                                                           . bit 4 b4 . bit 5 b5 . bit 6 b6 . bit 7 b7 $ zeroBits
    where bit :: Int -> Char -> Word8 -> Word8
          bit n '1' = flip setBit n
          bit _ '0' = id
          bit _  b  = error $ "bit must be 0 or 1; " ++ b:" given."

w :: String -> Word8
w = fromString

instance IsString (Stream [] a) where
  fromString = fromString' . filter (/= ' ')
    where fromString' :: String -> Stream [] a
          fromString' s | not . all (flip elem ['0', '1']) $ s = error $ "cannot create List Stream from " ++ s ++ "; elements must be 0 or 1"
                        | otherwise                            = let (ws, buff) = go s in S ws buff (length s)
          go :: String -> ([Word8], Buff)
          go s = let l = 8 * (length s `div` 8) in
                   (go' $ take l s, b $ drop l s)
          go' :: String -> [Word8]
          go' [] = []
          go' (b0:b1:b2:b3:b4:b5:b6:b7:rest) = let word = bit 0 b0 . bit 1 b1 . bit 2 b2 . bit 3 b3
                                                          . bit 4 b4 . bit 5 b5 . bit 6 b6 . bit 7 b7 $ zeroBits
                                               in word : go' rest
          go' s = error $ "cannot creates [Word8] from " ++ s ++ "; must be multiple of 8 chars."
          
          bit :: Int -> Char -> Word8 -> Word8
          bit n '1' = flip setBit n
          bit _ '0' = id
          bit _  b  = error $ "bit must be 0 or 1; " ++ b:" given."  

ls :: String -> Stream [] Word8
ls = fromString

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
    it "has an IsString isntance" $ do
      ls "" `shouldBe` (S [] nullBuff 0) 
      ls "1" `shouldBe` (S [] (Buff (1,0b00000001)) 1)
      ls "10101010 101" `shouldBe` (S [0b01010101] (Buff (3, 0b00000101)) 11)

spec_stream :: Spec
spec_stream = do
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

  describe "Bitstream" $ do
    it "should produce word aligned results" $ do
      execBitstream 0 (pure ()) `shouldBe` []
      execBitstream 0 (emitBit False) `shouldBe` [0b00000000]
      execBitstream 0 (emitBit True) `shouldBe`  [0b00000001]
      execBitstream 0 (emitBit True >> emitBit False >> emitBit True) `shouldBe` [0b00000101]