module Test.Dns.Decode (test_DnsDecode) where

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.ByteString
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Word (Word16)

import DNS

test_DnsDecode :: Tasty.TestTree
test_DnsDecode =
  Tasty.testGroup
    "DNS Decode"
    [ THH.testProperty "serialize/deserialize is idempotent" $
        HH.withDiscards 5000 . HH.property $ do
          flags <- HH.forAll $ Gen.word16 Range.constantBounded
          case parseOnly parseFlags $ toBytes flags of
            Left _ -> HH.discard
            Right f -> serializeFlags f === flags
    , THH.testProperty "encode/decode domain names is idempotent" $
        HH.property $ do
          domainName <- HH.forAll genDomainName
          (decodeName . encodeName) domainName === domainName
    , THH.testProperty "Should parse domain names from www.facebook.com response" $
        HH.property $ do
          packet <- liftIO $ BS.readFile "test/responses/www.facebook.com"
          decodeNameWithPacket packet (BS.drop 0x0C packet) === "www.facebook.com"
          decodeNameWithPacket packet (BS.drop 0x2E packet) === "star-mini.c10r.facebook.com"
    ]

genDomainName :: HH.MonadGen m => m BS.ByteString
genDomainName = BS.intercalate "." <$> Gen.list (Range.constant 1 10) genPart
  where
    genPart =
      BS.pack . map (fromIntegral . ord)
        <$> Gen.list (Range.constant 1 63) Gen.alpha

encodeName :: BS.ByteString -> BS.ByteString
encodeName = BS.toStrict . BSB.toLazyByteString . encodeDnsName

decodeNameWithPacket :: BS.ByteString -> BS.ByteString -> BS.ByteString
decodeNameWithPacket packet = either error id . parseOnly (parseDomainName packet)

decodeName :: BS.ByteString -> BS.ByteString
decodeName = decodeNameWithPacket BS.empty

toBytes :: Word16 -> BS.ByteString
toBytes w = BS.pack $ map fromIntegral [w `shiftR` 8, w .&. 0x00FF]
