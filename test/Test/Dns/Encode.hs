{-# LANGUAGE BinaryLiterals #-}

module Test.Dns.Encode (test_DnsEncode) where

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

import qualified Data.ByteString.Builder as BSB

import DNS

test_DnsEncode :: Tasty.TestTree
test_DnsEncode =
  Tasty.testGroup
    "DNS Encode"
    [ THH.testProperty "Should encode example.com" $
        HH.property $ do
          let result =
                BSB.word8 7
                  <> BSB.byteString "example"
                  <> BSB.word8 3
                  <> BSB.byteString "com"
                  <> BSB.word8 0x00
          BSB.toLazyByteString (encodeDnsName "example.com")
            === BSB.toLazyByteString result
    , THH.testProperty "Should encode www.ietf.org" $
        HH.property $ do
          let result =
                BSB.word8 3
                  <> BSB.byteString "www"
                  <> BSB.word8 4
                  <> BSB.byteString "ietf"
                  <> BSB.word8 3
                  <> BSB.byteString "org"
                  <> BSB.word8 0x00
          BSB.toLazyByteString (encodeDnsName "www.ietf.org")
            === BSB.toLazyByteString result
    , THH.testProperty "Should encode question with www.ietf.org" $
        HH.property $ do
          let result =
                BSB.word8 3
                  <> BSB.byteString "www"
                  <> BSB.word8 4
                  <> BSB.byteString "ietf"
                  <> BSB.word8 3
                  <> BSB.byteString "org"
                  <> BSB.word8 0x00
                  <> BSB.word16BE 28
                  <> BSB.word16BE 1
          BSB.toLazyByteString (serializeQuestion $ DNSQuestion "www.ietf.org" AAAA 1)
            === BSB.toLazyByteString result
    , THH.testProperty "Should encode null flags" $
        HH.property $ do
          let fs =
                DNSHeaderFlags
                  { qr = Query
                  , opCode = 0
                  , recursive = False
                  , authoritativeAnswers = False
                  , recursionAvailable = False
                  , tc = False
                  , rCode = Success
                  }
          serializeFlags fs === 0b0000000000000000
    , THH.testProperty "Should encode QR properly" $
        HH.property $ do
          let fs =
                DNSHeaderFlags
                  { qr = Response
                  , opCode = 0
                  , recursive = False
                  , authoritativeAnswers = False
                  , recursionAvailable = False
                  , tc = False
                  , rCode = Success
                  }
          serializeFlags fs === 0b1000000000000000
    , THH.testProperty "Should encode QR, OP Code properly" $
        HH.property $ do
          let fs =
                DNSHeaderFlags
                  { qr = Response
                  , opCode = 0x9
                  , recursive = False
                  , authoritativeAnswers = False
                  , recursionAvailable = False
                  , tc = False
                  , rCode = Success
                  }
          serializeFlags fs === 0b1100100000000000
    , THH.testProperty "Should encode QR, OP Code, AA properly" $
        HH.property $ do
          let fs =
                DNSHeaderFlags
                  { qr = Response
                  , opCode = 0x9
                  , recursive = False
                  , authoritativeAnswers = True
                  , recursionAvailable = False
                  , tc = False
                  , rCode = Success
                  }
          serializeFlags fs === 0b1100110000000000
    , THH.testProperty "Should encode QR, OP Code, AA, TC properly" $
        HH.property $ do
          let fs =
                DNSHeaderFlags
                  { qr = Response
                  , opCode = 0x9
                  , recursive = False
                  , authoritativeAnswers = True
                  , recursionAvailable = False
                  , tc = True
                  , rCode = Success
                  }
          serializeFlags fs === 0b1100111000000000
    , THH.testProperty "Should encode QR, OP Code, AA, TC, RD properly" $
        HH.property $ do
          let fs =
                DNSHeaderFlags
                  { qr = Response
                  , opCode = 0x9
                  , recursive = True
                  , authoritativeAnswers = True
                  , recursionAvailable = False
                  , tc = True
                  , rCode = Success
                  }
          serializeFlags fs === 0b1100111100000000
    , THH.testProperty "Should encode QR, OP Code, AA, TC, RD, RA properly" $
        HH.property $ do
          let fs =
                DNSHeaderFlags
                  { qr = Response
                  , opCode = 0x9
                  , recursive = True
                  , authoritativeAnswers = True
                  , recursionAvailable = True
                  , tc = True
                  , rCode = Success
                  }
          serializeFlags fs === 0b1100111110000000
    , THH.testProperty "Should encode QR, OP Code, AA, TC, RD, RA, RCode properly" $
        HH.property $ do
          let fs =
                DNSHeaderFlags
                  { qr = Response
                  , opCode = 0x9
                  , recursive = True
                  , authoritativeAnswers = True
                  , recursionAvailable = True
                  , tc = True
                  , rCode = NXDomain
                  }
          serializeFlags fs === 0b1100111110000011
    , THH.testProperty "Should encode header properly" $
        HH.property $ do
          let hdrFlags =
                DNSHeaderFlags
                  { qr = Response
                  , opCode = 0x9
                  , recursive = True
                  , authoritativeAnswers = True
                  , recursionAvailable = True
                  , tc = True
                  , rCode = NXDomain
                  }
              hdr =
                DNSHeader
                  { headerId = 0xCB01
                  , flags = hdrFlags
                  , numQuestions = 1
                  , numAnswers = 0
                  , numAuthorities = 1
                  , numAdditionals = 0
                  }
              result =
                BSB.toLazyByteString $
                  BSB.word16BE 0xCB01
                    <> BSB.word16BE 0b1100111110000011
                    <> BSB.word16BE 1
                    <> BSB.word16BE 0
                    <> BSB.word16BE 1
                    <> BSB.word16BE 0
          BSB.toLazyByteString (serializeHeader hdr) === result
    , THH.testProperty "Should encode query properly" $
        HH.property $ do
          let result =
                BSB.toLazyByteString $
                  BSB.word16BE 0xCB01
                    <> BSB.word16BE 0b0000000000000000
                    <> BSB.word16BE 1
                    <> BSB.word16BE 0
                    <> BSB.word16BE 0
                    <> BSB.word16BE 0
                    <> BSB.word8 7
                    <> BSB.byteString "example"
                    <> BSB.word8 3
                    <> BSB.byteString "com"
                    <> BSB.word8 0x00
                    <> BSB.word16BE 1
                    <> BSB.word16BE 1
          serializeQuery 0xCB01 "example.com" A === result
    ]
