{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module DNS.Decode
  ( parsePacket
  , parseHeader
  , parseQuestion
  , parseRecord
  , parseDomainName
  , parseFlags
  , isZSet
  )
where

import Control.Applicative ((<|>))
import Control.Monad (mzero, when)
import Data.Attoparsec.ByteString
import Data.Attoparsec.Helper
import Data.Bits (shiftR, testBit, clearBit)
import Data.Bits.Helper
import Data.List (intercalate)
import qualified Data.ByteString as BS
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
        <$> count h.numQuestions parseQuestion
        <*> count h.numAnswers (parseRecord packet)
        <*> count h.numAuthorities (parseRecord packet)
        <*> count h.numAdditionals (parseRecord packet)

parseQuestion :: Parser DNSQuestion
parseQuestion = do
  DNSQuestion
    <$> parseDomainName BS.empty -- Question names aren't compressed
    <*> (either fail pure . idToDNSRequestType =<< anyWord16BE)
    <*> (fromIntegral <$> anyWord16BE)

parseRecord :: BS.ByteString -> Parser DNSRecord
parseRecord packet = do
  rName <- parseDomainName packet
  rType <- either fail pure . idToDNSRequestType =<< anyWord16BE
  DNSRecord rName rType
    <$> (fromIntegral <$> anyWord16BE)
    <*> (fromIntegral <$> anyWord32BE)
    <*> parseDataType rType
  where
    parseDataType t = do
      bs <- take . fromIntegral =<< anyWord16BE
      case t of
        A -> pure $ IPv4 (toIPv4BS bs)
        AAAA -> pure $ Undefined bs
        TXT -> pure $ Undefined bs
        CNAME -> Cname <$> parseCname bs
        NS -> pure $ Undefined bs
        MX -> pure $ Undefined bs
        SOA -> pure $ Undefined bs

    toIPv4BS = intercalate "." . map show . BS.unpack
    parseCname = either fail pure . parseOnly (parseDomainName packet)

parseDomainName :: BS.ByteString -> Parser BS.ByteString
parseDomainName packet =
  parseLabelsAndCompressed
    <|> parseFromPointer
    <|> (parseLabelsWithoutNull <* anyWord8)
  where
    parseLabelsAndCompressed = do
      labels <- parseLabelsWithoutNull
      ptr <- parseFromPointer
      pure $ labels <> "." <> ptr

    parseLabelsWithoutNull = BS.intercalate "." <$> many1' parseLabel

    parseLabel = do
      l <- fromIntegral <$> anyWord8
      if l == 0 then mzero else take l

    parseFromPointer = do
      l <- fromIntegral <$> anyWord16BE
      if isPointer l
        then subParser (parseDomainName packet) (BS.drop (mkPointer l) packet)
        else mzero

    isPointer w = all (testBit w) [14, 15]
    mkPointer w = foldr (flip clearBit) w [14, 15]

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
