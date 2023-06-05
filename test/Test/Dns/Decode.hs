module Test.Dns.Decode (test_DnsDecode) where

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as THH

import Data.Attoparsec.ByteString
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
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
    ]

toBytes :: Word16 -> BS.ByteString
toBytes w = BS.pack $ map fromIntegral [w `shiftR` 8, w .&. 0x00FF]
