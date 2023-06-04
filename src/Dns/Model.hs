module Dns.Model
  ( DNSHeader (..)
  , DNSHeaderFlags (..)
  , QR (..)
  , RCode (..)
  , DNSQuestion (..)
  , DNSRequestType (..)
  , defaultFlags
  , defaultHeader
  , dnsRequestTypeId
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
