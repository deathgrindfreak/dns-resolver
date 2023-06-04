{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Dns.Parser
  ( serializeQuery
  , serializeHeader
  , serializeFlags
  , serializeQuestion
  , encodeDnsName
  )
where

import Data.Bits (shiftL, (.|.))
import Data.Bits.Helper
import Data.Word (Word16, Word8)
import Dns.Model

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS

classIn :: Int
classIn = 1

serializeQuery ::
  Int ->
  BS.ByteString ->
  DNSRequestType ->
  LBS.ByteString
serializeQuery headerId name recordType =
  let header = defaultHeader {headerId, numQuestions = 1}
      question = DNSQuestion {name, type_ = recordType, class_ = classIn}
   in BSB.toLazyByteString $
        serializeHeader header <> serializeQuestion question

serializeHeader :: DNSHeader Int -> BSB.Builder
serializeHeader header =
  foldMap
    (BSB.word16BE . fromIntegral)
    [ header.headerId
    , fromIntegral $ serializeFlags header.flags
    , header.numQuestions
    , header.numAnswers
    , header.numAuthorities
    , header.numAdditionals
    ]

serializeFlags :: DNSHeaderFlags -> Word16
serializeFlags fs =
  foldr
    (.|.)
    0
    [ serializeRCode fs.rCode
    , set fs.recursionAvailable `shiftL` 7
    , set fs.recursive `shiftL` 8
    , set fs.tc `shiftL` 9
    , set fs.authoritativeAnswers `shiftL` 10
    , mask 4 (fromIntegral fs.opCode) `shiftL` 11
    , set (fs.qr == Response) `shiftL` 15
    ]
  where
    serializeRCode r =
      case r of
        Success -> 0
        ServFail -> 2
        NXDomain -> 3

    set b = if b then 1 else 0

serializeQuestion :: DNSQuestion -> BSB.Builder
serializeQuestion qstn =
  encodeDnsName qstn.name
    <> (BSB.word16BE . fromIntegral . dnsRequestTypeId) qstn.type_
    <> (BSB.word16BE . fromIntegral) qstn.class_

encodeDnsName :: BS.ByteString -> BSB.Builder
encodeDnsName name =
  foldMap
    (\e -> BSB.word8 (fromIntegral $ BS.length e) <> BSB.byteString e)
    (BS.split dot name)
    <> BSB.word8 0

dot :: Word8
dot = (fromIntegral . fromEnum) '.'
