module Data.Bits.Helper (mask)
where

import Data.Bits

mask :: (Bits a, Num a) => Int -> a -> a
mask n = (complement ((complement 0 `shiftR` n) `shiftL` n) .&.)
