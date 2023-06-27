{-# LANGUAGE StrictData #-}

module DNS.Model
  ( DNSPacket (..)
  , DNSHeader (..)
  , DNSHeaderFlags (..)
  , DNSDataType (..)
  , DNSHostName
  , QR (..)
  , RCode (..)
  , DNSQuestion (..)
  , DNSRecordType (..)
  , DNSRecord (..)
  , IPv4Address (..)
  , defaultFlags
  , defaultHeader
  , dnsRequestTypeId
  , idToDNSRecordType
  )
where

import Data.List (intercalate)
import qualified Data.ByteString as BS
import Data.Word (Word16)

defaultFlags :: DNSHeaderFlags
defaultFlags =
  DNSHeaderFlags Query 0 False False False False Success

defaultHeader :: DNSHeader ()
defaultHeader = DNSHeader () defaultFlags 0 0 0 0

data DNSPacket = DNSPacket
  { header :: DNSHeader Int
  , questions :: [DNSQuestion]
  , answers :: [DNSRecord]
  , authorities :: [DNSRecord]
  , additionals :: [DNSRecord]
  }
  deriving (Show)

data QR = Query | Response deriving (Eq, Enum, Show)
data RCode = Success | ServFail | NXDomain deriving (Eq, Enum, Show)

data DNSHeaderFlags = DNSHeaderFlags
  { qr :: QR
  , opCode :: Int
  , recursive :: Bool
  , authoritativeAnswers :: Bool
  , recursionAvailable :: Bool
  , tc :: Bool
  , rCode :: RCode
  }
  deriving (Show)

data DNSHeader a = DNSHeader
  { headerId :: a
  , flags :: DNSHeaderFlags
  , numQuestions :: Int
  , numAnswers :: Int
  , numAuthorities :: Int
  , numAdditionals :: Int
  }
  deriving (Show)

data DNSQuestion = DNSQuestion
  { questionName :: DNSHostName
  , questionType :: DNSRecordType
  , questionClass :: Int
  }
  deriving (Show)

data DNSRecord = DNSRecord
  { recordName :: DNSHostName
  , recordType :: DNSRecordType
  , recordClass :: Int
  , recordTTL :: Int
  , recordData :: DNSDataType
  }
  deriving (Show)

type DNSHostName = BS.ByteString

newtype IPv4Address = IPv4Address
  { ipv4AddressToByteString :: BS.ByteString }
  deriving (Eq, Ord)

instance Show IPv4Address where
  show = intercalate "." . map show . BS.unpack . ipv4AddressToByteString

data DNSDataType
  = IPv4 IPv4Address
  | IPv6 String
  | Cname DNSHostName
  | Text String
  | NameServer DNSHostName
  | Undefined BS.ByteString
  deriving (Show)

data DNSRecordType
  = A
  | AAAA
  | TXT
  | CNAME
  | NS
  | MX
  | SOA
  deriving (Show, Read, Eq, Enum)

dnsRequestTypeId :: DNSRecordType -> Word16
dnsRequestTypeId tp =
  case tp of
    A -> 1
    AAAA -> 28
    TXT -> 16
    CNAME -> 5
    NS -> 2
    MX -> 15
    SOA -> 6

idToDNSRecordType :: Word16 -> Either String DNSRecordType
idToDNSRecordType tp =
  case tp of
    1 -> Right A
    28 -> Right AAAA
    16 -> Right TXT
    5 -> Right CNAME
    2 -> Right NS
    15 -> Right MX
    6 -> Right SOA
    r -> Left $ "Unknown request type " <> show r
