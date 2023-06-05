module Data.Attoparsec.Helper
  ( beInt
  , anyWord16BE
  )
where

import Data.Attoparsec.ByteString
import Data.Word (Word16, Word8)

byteWidth :: Int
byteWidth = 16 ^ (2 :: Int)

beInt :: Int -> Parser Int
beInt n = wordsToBEInt <$> count n anyWord8

anyWord16BE :: Parser Word16
anyWord16BE = fromIntegral . wordsToBEInt <$> count 2 anyWord8

wordsToBEInt :: [Word8] -> Int
wordsToBEInt = foldr1 ((+) . (byteWidth *)) . map fromIntegral
