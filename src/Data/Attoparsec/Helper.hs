module Data.Attoparsec.Helper
  ( beInt
  , wordsToBEInt
  , anyWord16BE
  , anyWord32BE
  , subParser
  )
where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word8)

byteWidth :: Int
byteWidth = 16 ^ (2 :: Int)

beInt :: Int -> Parser Int
beInt n = wordsToBEInt <$> count n anyWord8

anyWord16BE :: Parser Word16
anyWord16BE = fromIntegral . wordsToBEInt <$> count 2 anyWord8

anyWord32BE :: Parser Word32
anyWord32BE = fromIntegral . wordsToBEInt <$> count 4 anyWord8

wordsToBEInt :: [Word8] -> Int
wordsToBEInt = foldr1 ((+) . (byteWidth *)) . map fromIntegral

subParser :: Parser a -> BS.ByteString -> Parser a
subParser p b = either fail pure $ parseOnly p b
