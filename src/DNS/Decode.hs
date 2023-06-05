{-# LANGUAGE NamedFieldPuns #-}

module DNS.Decode
  ( parseHeader
  , parseFlags
  , isZSet
  )
where

import Control.Monad (when)
import Data.Attoparsec.ByteString
import Data.Attoparsec.Helper
import Data.Bits (shiftR, testBit)
import Data.Bits.Helper
import Data.Word (Word16)

import DNS.Model

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

  when (isZSet flagNum) $ do
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
