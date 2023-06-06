{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DNS.Decode
  ( parsePacket
  , parseHeader
  , parseQuestion
  , parseRecord
  , parseFlags
  , isZSet
  )
where

import Control.Monad (mzero, when)
import Data.Attoparsec.ByteString
import Data.Attoparsec.Helper
import Data.Bits (shiftR, testBit, (.&.))
import Data.Bits.Helper
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Word (Word16)
import Prelude hiding (take)

import DNS.Model

parsePacket :: Parser DNSPacket
parsePacket = do
  mbPacket <- getChunk
  case mbPacket of
    Nothing -> fail "parseResponse reached end of input"
    Just packet -> do
      h <- parseHeader
      DNSPacket h
        <$> (count h.numQuestions parseQuestion)
        <*> (count h.numAnswers (parseRecord packet))
        <*> (count h.numAuthorities (parseRecord packet))
        <*> (count h.numAdditionals (parseRecord packet))

parseQuestion :: Parser DNSQuestion
parseQuestion = do
  DNSQuestion
    <$> parseName BS.empty -- Question names aren't compressed
    <*> (either fail pure . idToDNSRequestType =<< anyWord16BE)
    <*> (fromIntegral <$> anyWord16BE)

parseRecord :: BS.ByteString -> Parser DNSRecord
parseRecord packet =
  DNSRecord
    <$> parseName packet
    <*> (either fail pure . idToDNSRequestType =<< anyWord16BE)
    <*> (fromIntegral <$> anyWord16BE)
    <*> (fromIntegral <$> anyWord32BE)
    <*> (take . fromIntegral =<< anyWord16BE)

parseName :: BS.ByteString -> Parser BS.ByteString
parseName packet = do
  parts <- partitionEithers <$> many1' parsePart
  parts' <- case parts of
              ([], rs) -> anyWord8 *> pure rs
              (ls, []) -> pure ls
              _ -> fail "Mixed compressed and uncompressed parts"
  pure $ BS.intercalate "." parts'
  where
    parsePart =
      anyWord8 >>= \case
        0 -> mzero
        l | l .&. 0b1100_0000 /= 0 -> Left <$> parseCompressed l
        l -> Right <$> take (fromIntegral l)

    parseCompressed l = do
      l' <- fromIntegral . ((l .&. 0b0011_1111) +) <$> anyWord8
      subParser (parseName packet) (BS.drop l' packet)

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
