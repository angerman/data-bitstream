{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, KindSignatures, BinaryLiterals, RecursiveDo, LambdaCase, BangPatterns, FlexibleInstances #-}
module Data.Bitstream
  ( Buff, nullBuff, addBuff, mkBuff
  , Bitstream, execBitstream, evalBitstream, runBitstream
  -- * Monadic interface
  , withOffset
  -- ** locations
  , loc, locBytes, locWords
  -- ** Bits
  , emitBit, emitBits
  -- ** Words
  , emitWord8, emitWord32R, emitWord32
  -- ** Others
  , emitFixed, emitVBR, emitChar6
  -- ** alignment
  , alignWord8, alignWord32
  -- ** writing
  , writeFile, withHeader
  -- ** For testing
  , Stream(..), Buff(..), Bitstream(..)
  -- , TStream(..), buffSize
  ) where

import Prelude hiding (last, words, writeFile, tail)

import Data.Word
import Data.Bits

import qualified Data.List as L
import qualified Data.ByteString as B

import Data.Monoid ((<>))
import Control.Monad.Fix

import GHC.Stack (HasCallStack)


import Data.ContBuff


-- | The position in the stream. 
type Position = Int

-- | A @Word8@ buffer, tracking the number of bits.
newtype Buff = Buff (Int, Word8) deriving (Eq, Ord)

nullBuff :: Buff
nullBuff = Buff (0,0)

-- | Adding two buffers. E.g. copying over the bits from one buffer into the
-- other. If we complete a @Word8@, this will be in the first position of the
-- result.
--
-- The bitcode stream will look like
--
-- 0      8 9     16 17    32
-- |      | |      | |      |
-- 10101111 00001100 10101010
--
-- The on disk representation will however look like
--
-- 11110101 00110000 01010101
-- |      | |      | |      |
-- 8      0 16     9 32    17
--
-- Thus, to now have to rearange the bits before writing to disk, we will
-- encode them in disk order (as opposed to stream order) from the start.
--
-- Assume we have two buffers (1, 0b00000001) and (1, 0b00000001), we expect
-- to obtain (2, 0b00000011). Thus we want to shift the second buffer by the
-- length of the first, and or them.
--
-- In the spill case (5, 0b00010101) and (4, 0b00001111) we expect to get
-- (Just 0b11110101, (1, b000000001)) 
--
addBuff :: Buff -> Buff -> (Maybe Word8, Buff)
addBuff (Buff (n,w)) (Buff (n',w')) | n+n' < 8  = (Nothing
                                                  , Buff (n+n', w .|. (shift w' n)))
                                    | otherwise = (Just (w .|. (shift w' n))
                                                  , Buff ((n+n') `mod` 8, shift w' (n-8)))


-- | Smart constructor for @Buff@. Ensures that
-- the stored byte is masked properly.
mkBuff :: Int -> Word8 -> Buff
mkBuff n w = Buff (n, mask n w)

-- | A stream is a number of Words, a buffer and a position (length of the stream) marker.
data Stream f a = S
  { _words  :: f Word8
  , _buffer :: Buff
  , _len    :: Position
  }


deriving instance Eq (f Word8) => Eq (Stream f a)
deriving instance Ord (f Word8) => Ord (Stream f a)

-- instance ( Monoid (f Word8)
--         , Foldable f
--         , Traversable f
--         , Applicative f) => Monoid (Stream f a) where
instance Monoid (Stream [] a) where
  mempty = S mempty nullBuff 0
  lhs `mappend` (S _ _ 0) = lhs
  (S _ _ 0) `mappend` rhs = rhs
  (S w b p) `mappend` (S w' b' p') = case b of
    -- there are no bits in the buffer. We can simply
    -- concatinate lhs and rhs
    Buff (0,_) -> S (w <> w') b' (p+p')
    -- there are already @n@ bites in the buffer. We will
    -- need to shift all the bits in the RHS left by 8-n.
    Buff (n,c) | null w' -> case addBuff b b' of
                               (Just w'', b'') -> S (w <> pure w'') b'' (p+p')
                               (Nothing,  b'') -> S w b'' (p+p')
               | otherwise -> let (l, w'') = L.mapAccumL (go' n) c w'
                              in case addBuff (Buff (n, l)) b' of
                                   (Just w''', b'') -> S (w <> w'' <> pure w''') b'' (p+p')
                                   (Nothing,   b'') -> S (w <> w'')              b'' (p+p')
      where
        -- go :: (Monoid (t Word8), Applicative t, Foldable t)
        --       => Int -> (t Word8, Word8) -> Word8 -> (t Word8, Word8)
        --    go n (acc, b) b' = (acc <> pure (b .|. shift b' n), shift b' (n-8))

            go' :: Int    -- ^ shift
                -> Word8  -- ^ buff
                -> Word8  -- ^ input
                -> ( Word8   -- ^ new buff
                   , Word8 ) -- ^ output
            go' n b w = (shift w (n-8), b .|. shift w n) 

-- mappend is not cheap here.
type ListStream = BList Word8
-- type ListStream = Stream [] Word8

class HasWords a where
  getWords :: a -> [Word8]

instance HasWords (Stream [] Word8) where
  getWords = _words

instance HasWords (BList Word8) where
  getWords (BL xs) = map extractWord xs
    where extractWord :: Buffer Word8 -> Word8
          extractWord (Full w) = w
          extractWord (Partial _ w) = w

class ToStream a where
  mkStream :: [Word8] -> Int -> Word8 -> Position -> a

instance ToStream (Stream [] Word8) where
  mkStream ws 0 _ p = S ws nullBuff p
  mkStream ws n b p = S ws (mkBuff n b) p

instance ToStream (BList Word8) where
  mkStream [] 0 _ _ = BL $ []
  mkStream ws 0 _ _ = BL $ map Full ws
  mkStream [] n b _ = BL $ [Partial n $ mask n b]
  mkStream ws n b _ = BL $ go ws n b
    where go []     0 _ = []
          go []     n b = [Partial n $ mask n b]
          go (w:ws) n b = Full w:go ws n b
  

newtype Bitstream a = Bitstream { unBitstream :: Position -> (ListStream, Position, a) }

stream :: (ListStream, Position, a) -> ListStream
stream (s,_,_) = s
position :: (ListStream, Position, a) -> Position
position (_,p,_) = p
value :: (ListStream, Position, a) -> a
value (_,_,v) = v

runBitstream :: Position -> Bitstream a -> (ListStream, Position, a)
runBitstream p (Bitstream f) = f p
execBitstream :: Position -> Bitstream a -> [Word8]
execBitstream p a = getWords . stream . runBitstream p $ a >> alignWord8
evalBitstream :: Position -> Bitstream a -> a
evalBitstream p = value . runBitstream p

-- * Functor
instance Functor Bitstream where
  fmap f x = Bitstream $ \pos -> let
    (s, pos', x') = unBitstream x pos
    in (s, pos', f x')

-- * Applicative
instance Applicative Bitstream where
  pure r = Bitstream $ \pos -> (mkStream mempty 0 0 0, pos, r)
  x <*> y = Bitstream $ \pos -> let
    (s, pos', f) = unBitstream x pos
    (s', pos'', a) = unBitstream y pos'
    in (s <> s', pos'', f a)

-- * Monad
instance Monad Bitstream where
  return r = Bitstream $ \pos -> (mkStream mempty 0 0 0, pos, r)
  x >>= y = Bitstream $ \pos -> let
    (s, pos', res) = unBitstream x pos
    (s', pos'', res') = unBitstream (y res) pos'
    in (s <> s', pos'', res')

-- * MonadFix
instance MonadFix Bitstream where
  mfix f = Bitstream $ \pos -> let
    (s, pos', r) = unBitstream (f r) pos
    in (s, pos', r)

-- Monadic Bitstream API

withOffset :: Int -> Bitstream a -> Bitstream a
withOffset n x = Bitstream $ \pos -> let
  (s, pos', r) = unBitstream x n
  in (s, pos+pos', r)

loc :: Bitstream Position
loc = Bitstream $ \pos -> (mempty, pos, pos)

locBytes :: Bitstream Word32
locBytes = Bitstream $ \pos -> (mkStream mempty 0 0 pos, pos, fromIntegral $ pos `div` 8)

locWords :: Bitstream Word32
locWords = Bitstream $ \pos -> (mkStream mempty 0 0 pos, pos, fromIntegral $ pos `div` 32)

emitBit :: Bool -> Bitstream ()
emitBit True  = Bitstream $ \pos -> (mkStream mempty 1 1 1, pos+1, ())
emitBit False = Bitstream $ \pos -> (mkStream mempty 1 0 1, pos+1, ())

emitBits :: Int -> Word8 -> Bitstream ()
emitBits 0 _ = pure ()
emitBits n b | n < 8 = Bitstream $ \pos -> (mkStream mempty n b n, pos+n, ())
             | otherwise = error $ "cannot emit " ++ show n ++ " bits from Word8."

emitWord8 :: Word8 -> Bitstream ()
emitWord8 w = Bitstream $ \pos -> (mkStream [w] 0 0 8, pos+8, ())

emitWord32R :: Word32 -> Bitstream ()
emitWord32R w = Bitstream $ \pos -> (mkStream [fromIntegral (shift w (-24))
                                              ,fromIntegral (shift w (-16))
                                              ,fromIntegral (shift w  (-8))
                                              ,fromIntegral w] 0 0 32, pos+32, ())
emitWord32 :: Word32 -> Bitstream ()
emitWord32 w = Bitstream $ \pos -> (mkStream [fromIntegral (shift w  (-0))
                                             ,fromIntegral (shift w  (-8))
                                             ,fromIntegral (shift w (-16))
                                             ,fromIntegral (shift w (-24))] 0 0 32, pos+32, ())

emitFixed :: Word64 -> Word64 -> Bitstream ()
emitFixed 0 _ = pure ()
emitFixed n w | n < 8  = Bitstream $ \pos -> (mkStream mempty       n'     (off  0 w) n', pos+n', ())
              | n < 16 = Bitstream $ \pos -> (mkStream [off  0 w]  (n'-8)  (off  8 w) n', pos+n', ())
              | n < 24 = Bitstream $ \pos -> (mkStream [off  0 w
                                                       ,off  8 w]  (n'-16) (off 16 w) n', pos+n', ())
              | n < 32 = Bitstream $ \pos -> (mkStream [off  0 w
                                                       ,off  8 w
                                                       ,off 16 w]  (n'-24) (off 24 w) n', pos+n', ())
              | n < 40 = Bitstream $ \pos -> (mkStream [off  0 w
                                                       ,off  8 w
                                                       ,off 16 w
                                                       ,off 24 w]  (n'-32) (off 32 w) n', pos+n', ())
              | n < 48 = Bitstream $ \pos -> (mkStream [off  0 w
                                                       ,off  8 w
                                                       ,off 16 w
                                                       ,off 24 w
                                                       ,off 32 w]  (n'-40) (off 40 w) n', pos+n', ())
              | n < 56 = Bitstream $ \pos -> (mkStream [off  0 w
                                                       ,off  8 w
                                                       ,off 16 w
                                                       ,off 24 w
                                                       ,off 32 w
                                                       ,off 40 w]  (n'-48) (off 48 w) n', pos+n', ())
              | n < 64 = Bitstream $ \pos -> (mkStream [off  0 w
                                                       ,off  8 w
                                                       ,off 16 w
                                                       ,off 24 w
                                                       ,off 32 w
                                                       ,off 40 w
                                                       ,off 48 w] (n'-56) (off 56 w) n', pos+n', ())
              | n == 64 = Bitstream $ \pos -> (mkStream [ off  0 w, off  8 w, off 16 w, off 24 w
                                                        , off 32 w, off 40 w, off 48 w, off 56 w]
                                               0 0
                                               64, pos+n', ())
              | otherwise = error $ "invalid number of bits. Cannot emit " ++ show n ++ " bits from Word64."
    where off :: Int -> Word64 -> Word8
          off n w = fromIntegral (shift w (-n))
          n' = fromIntegral n

emitVBR :: HasCallStack => Word64 -> Word64 -> Bitstream ()
-- emitVBR 0 _ = pure ()
emitVBR n _ | n < 2 = error "emitting VBR 0 impossible."
emitVBR n w = do
  emitFixed (n-1) w
  let tail = shift w (1-(fromIntegral n))
    in if popCount tail == 0
       then emitBit False
       else emitBit True >> emitVBR n tail

emitChar6 :: Char -> Bitstream ()
emitChar6 '_' = emitBits 6 63
emitChar6 '.' = emitBits 6 62
emitChar6 c | 'a' <= c && c <= 'z' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum 'a')
            | 'A' <= c && c <= 'Z' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum 'A') + 26
            | '0' <= c && c <= '9' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum '0') + 52
            | otherwise = fail $ "char '" ++ c:"' not in [a-zA-Z0-9._]"

alignWord8 :: Bitstream ()
alignWord8 = flip mod 8 <$> loc >>= \case
  0 -> pure ()
  x -> emitBits (8 - x) 0

alignWord32 :: Bitstream ()
alignWord32 = flip mod 32 <$> loc >>= \case
  0 -> pure ()
  x | 32 - x < 8  -> emitBits (32 - x) 0
  x | 32 - x < 16 -> emitWord8 0 >> emitBits (24 - x) 0
  x | 32 - x < 24 -> emitWord8 0 >> emitWord8 0 >> emitBits (16 - x) 0
  x | 32 - x < 32 -> emitWord8 0 >> emitWord8 0 >> emitWord8 0 >> emitBits (8 - x) 0

writeFile
  :: HasCallStack
  => FilePath -> Bitstream a -> IO ()
writeFile f = B.writeFile f . B.pack . execBitstream 0 

-- * BitCode Header
-- | put the BitCodeHeader, on darwin a special wrapper is
-- apparently only required, to make it compatible with
-- the system archiver.
withHeader
  :: HasCallStack
  => Bool    -- ^ wrap in darwin header
  -> Bitstream () -- ^ body bitcode
  -> Bitstream ()
withHeader isDarwin body = mdo
  -- if it's darwin, we add the header with the length of the body
  -- (#words * 4 bytes) as well as the LLVM IR Header (4 bytes)
  if isDarwin
    then emitDarwinHeader n
    else pure ()
  -- start a temporary offset from 0. To get the size
  -- of he body.
  n <- withOffset 0 $ do
    emitLLVMIRHeader
    body
    alignWord32
    locBytes -- get the number of bytes emitted.

  return ()
  where emitDarwinHeader
          :: Word32 -- ^ number of bytes in body
          -> Bitstream ()
        emitDarwinHeader len = do
          emitWord32 0x0b17c0de                -- 0x0b17c0de   4
          emitWord32 0                         -- version: 0  +4
          emitWord32 20                        -- offset: 20  +4 <--.
          emitWord32 len                       -- length      +4    |
          emitWord32 cpuType                   --             +4 => 20 in total.
            where
              -- We are hardcoding x86_64 for now.
              cpuType :: Word32
              cpuType = 0x01000000 -- DARWIN_CPU_ARCH_ABI64
                      +          7 -- DARWIN_CPU_TYPE_X86(7),
                                   -- DARWIN_CPU_TYPE_ARM(12),
                                   -- DARWIN_CPU_TYPE_POWERPC(18)
        emitLLVMIRHeader :: Bitstream ()
        emitLLVMIRHeader = emitWord32R 0x4243c0de -- 'BC' 0xc0de

-- Show instances. These make parsing debug output much easier.

showWord8 :: Word8 -> String
showWord8 w = '0':'b':(map f $ [testBit w i | i <- [0..7]])
  where f True  = '1'
        f False = '0'

showWord8' :: Word8 -> String
showWord8' w = map f $ [testBit w i | i <- [0..7]]
  where f True = '1'
        f False = '0'

instance Show Buff where
  show (Buff (n, w)) = show n ++ " bits: " ++ showWord8 w

instance (Functor f, Foldable f) => Show (Stream f a) where
  show (S ws (Buff (n,b)) p) | null ws = show p ++ " bits: " ++ take n (showWord8' b)
                             | otherwise = show p ++ " bits: " ++ foldl1 (\x y -> x ++ " " ++ y) (fmap showWord8' ws) ++ " " ++ take n (showWord8' b)
                             
