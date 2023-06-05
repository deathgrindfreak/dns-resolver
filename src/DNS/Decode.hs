{-# LANGUAGE NamedFieldPuns #-}

module DNS.Decode
  ( parseResponse
  , parseHeader
  , parseQuestion
  , parseFlags
  , isZSet
  )
where

import Control.Monad (mzero, when)
import Data.Attoparsec.ByteString
import Data.Attoparsec.Helper
import Data.Bits (shiftR, testBit)
import Data.Bits.Helper
import qualified Data.ByteString as BS
import Data.Word (Word16)
import Prelude hiding (take)

import DNS.Model

parseResponse ::
  BS.ByteString ->
  Either String (DNSHeader Int, DNSQuestion)
parseResponse = parseOnly $ (,) <$> parseHeader <*> parseQuestion

parseQuestion :: Parser DNSQuestion
parseQuestion = do
  DNSQuestion
    <$> (BS.intercalate "." <$> many1' parsePart <* anyWord8)
    <*> (either fail pure . idToDNSRequestType =<< anyWord16BE)
    <*> (fromIntegral <$> anyWord16BE)
  where
    parsePart = do
      l <- fromIntegral <$> anyWord8
      if l == 0 then mzero else take l

parseHeader :: Parser (DNSHeader Int)
parseHeader =
  DNSHeader <$> be16 <*> parseFlags <*> be16 <*> be16 <*> be16 <*> be16
  where
    be16 = fromIntegral <$> anyWord16BE

parseFlags :: Parser DNSHeaderFlags
parseFlags = do
  flagNum <- anyWord16BE
  rCode <-
    case mask 4 flagNum of
      0 -> pure Success
      2 -> pure ServFail
      3 -> pure NXDomain
      r -> fail $ "Unknown RCODE " <> show r

  when (isZSet flagNum) $
    fail "Z bits cannot be set"

  pure $
    DNSHeaderFlags
      { rCode
      , recursionAvailable = flagNum `testBit` 7
      , recursive = flagNum `testBit` 8
      , tc = flagNum `testBit` 9
      , authoritativeAnswers = flagNum `testBit` 10
      , opCode = fromIntegral $ mask 4 (flagNum `shiftR` 11)
      , qr = if flagNum `testBit` 15 then Response else Query
      }

isZSet :: Word16 -> Bool
isZSet w = any (testBit w) [4 .. 6]
