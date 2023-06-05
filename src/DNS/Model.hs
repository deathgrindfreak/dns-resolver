module DNS.Model
  ( DNSHeader (..)
  , DNSHeaderFlags (..)
  , QR (..)
  , RCode (..)
  , DNSQuestion (..)
  , DNSRequestType (..)
  , DNSRecord (..)
  , defaultFlags
  , defaultHeader
  , dnsRequestTypeId
  , idToDNSRequestType
  )
where

import qualified Data.ByteString as BS

defaultFlags :: DNSHeaderFlags
defaultFlags =
  DNSHeaderFlags Query 0 True False False False Success

defaultHeader :: DNSHeader ()
defaultHeader = DNSHeader () defaultFlags 0 0 0 0

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
  { name :: BS.ByteString
  , type_ :: DNSRequestType
  , class_ :: Int
  }
  deriving (Show)

data DNSRecord = DNSRecord
  { recordName :: BS.ByteString
  , recordType :: DNSRequestType
  , recordClass :: Int
  , recordTTL :: Int
  , recordData :: BS.ByteString
  }

data DNSRequestType
  = A
  | AAAA
  | TXT
  | CNAME
  | NS
  | MX
  | SOA
  deriving (Show, Eq, Enum)

dnsRequestTypeId :: DNSRequestType -> Int
dnsRequestTypeId tp =
  case tp of
    A -> 1
    AAAA -> 28
    TXT -> 16
    CNAME -> 5
    NS -> 2
    MX -> 15
    SOA -> 6

idToDNSRequestType :: Int -> Either String DNSRequestType
idToDNSRequestType tp =
  case tp of
    1 -> Right A
    28 -> Right AAAA
    16 -> Right TXT
    5 -> Right CNAME
    2 -> Right NS
    15 -> Right MX
    6 -> Right SOA
    r -> Left $ "Unknown request type " <> show r
