{-# LANGUAGE FlexibleInstances, BinaryLiterals, ScopedTypeVariables #-}
module Data.ContBuff where

import Data.Bits
import qualified Data.List as L
import Data.Word (Word8,Word32,Word64)

import Debug.Trace

class ShowBinary a where
  showBin :: a -> String

mask :: (Num a, FiniteBits a) => Int -> a -> a
mask n w = m .&. w
  where m = setBit zeroBits (n+1) - 1
-- | given a buffer @b@ and a new word @w@ shift @w@ into @b@.
-- and return the new w' = b + w, b' = w shifted and masked.
--
join :: (Num a, FiniteBits a) => Int -> a -> a -> (a, a)
join n b w = (b .|. shift w n, shift w (n - finiteBitSize w))

-- z :: FiniteBits a
--  => (a -> a -> (a,a))     -- ^ y
--  -> Int                   -- ^ position in buffer
--  -> a                     -- ^ buffer
--  -> (TCont a)                 -- ^ continuation
--  -> TCont a
-- z f n b c = \l -> go b (c l)
--  where -- go :: a -> ([a], TBuff a) -> ([a], TBuff a)
--        go b ([], (n', b')) | n + n' < buffSize = ([],                   ( n+n',                 b .|. (shift b' n)))
--                            | otherwise         = ([b .|. (shift b' n)], ((n+n') `mod` buffSize, shift b' (n-buffSize))) 
--        go b (x:xs, y) = let (x',b') = f b x
--                             (xs',b'') = go b' (xs, y)
--                         in (x':xs', b'')
        
-- example:
-- z f 0b1 c (0b01010101:[]) = let (x',b') = f 0b1 0b1010101 in x':z f b' c []
-- z f b'  c [] = \c' -> z f b' c' (c [])

data Buffer a
  = Full a | Partial Int a
  deriving (Show, Eq, Ord)

newtype BList a = BL [Buffer a] deriving (Eq, Ord)

instance (Num a, FiniteBits a) => Monoid (BList a) where
  mempty = BL []
  BL xs `mappend` BL ys
    = let r = BL $ go xs ys
      in r -- trace (show (BL xs) ++ " <> " ++ show (BL ys) ++ " = " ++ show r) r 
    where go []                ys               = ys
          go (x@(Full _):xs)   ys               = x:go xs ys
          go (x@[Partial _ _]) []               = x
          go [Partial n x]     [Partial n' y]
            | n + n'  < finiteBitSize x         = [Partial (n + n') $ fst $ join n x y]
            | n + n' == finiteBitSize x         = [Full $ fst $ join n x y]
            | otherwise                         = let (w,b) = join n x y in [Full w, Partial ((n + n') `mod` finiteBitSize w) b]
          go [Partial n x]     (Full y:ys)      = let (w,b) = join n x y in Full w:go [Partial n b] ys

instance FiniteBits a => Show (BList a) where
  show (BL xs) = L.intercalate " " $ map show' xs
    where show' :: FiniteBits a => Buffer a -> String
          show' (Full w) = showFinite w
          show' (Partial i w) = take i $ showFinite w

-- instance Monoid TList Word8 where
--  mempty = TL ([],0)
--   TL (xs,t) `mappend` TS (xs',t')
--    = xs

-- | The @TStream@. We basically use a list of @a@ and it's tail.
-- newtype TStream a = TS (Int, TList -> TList)

-- instance FiniteBits a => Eq (TStream a) where
--  TS (w, p) == TS (w', p') = p == p' && w (0, []) == w' (0, []) 

-- concat :: FiniteBits a => TStream a -> TStream a -> TStream a
-- concat (TS (w, p)) (TS (w', p'))
--  = 


-- we will hardcode this for TStream Word8 for now.
buffSize :: Int
buffSize = 8

-- instance Monoid (TStream Word8) where
--  mempty = TS (id, 0)
--  TS (w,p) `mappend` TS (w',p') = undefined
--    = TS (cont, size)
--    where size = (p+p') `mod` buffSize
--          cont = \l -> 
--

showFinite :: FiniteBits a => a -> String
showFinite w = map f $ [testBit w i | i <- [0..finiteBitSize w - 1]]
  where f True = '1'
        f False = '0'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h,t) = splitAt n xs in h:chunksOf n t

-- instance Show (TStream a) where
--  show (TS (ws, p)) = show l ++ " bits: " ++ L.intercalate " " (map showFinite ws')
--    where (ws',l) = case ws [] of
--            (ws, (_, b)) | p == 0    -> (ws, 8 * length ws)
--                         | otherwise -> (ws ++ [b], 8 * length ws + p)
