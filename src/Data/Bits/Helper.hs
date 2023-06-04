module Data.Bits.Helper
  ( mask
  , flag
  )
where

import Data.Bits

mask :: (Bits a, Num a) => Int -> a -> a
mask n = (complement ((complement 0 `shiftR` n) `shiftL` n) .&.)

flag :: (Bits a, Num a) => Bool -> Int -> a
flag b n = if b then 1 `shiftL` n else 0
