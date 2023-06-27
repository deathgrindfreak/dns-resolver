module DNS.Query
  ( sendQuery
  , resolve
  , rootServer
  )
where

import Control.Exception (throwIO, Exception)
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Network.Run.UDP (runUDPClient)
import Network.Socket (HostName)
import Network.Socket.ByteString (recv, sendAllTo)
import System.Random

import DNS.Decode
import DNS.Encode
import DNS.Model

-- a.root-servers.net.
rootServer :: HostName
rootServer = "198.41.0.4"

data DNSError
  = DNSParsingError String
  | DNSResolveError
  deriving Show

instance Exception DNSError

sendQuery ::
  HostName ->
  DNSHostName ->
  DNSRecordType ->
  IO DNSPacket
sendQuery ipAddress domainName rt = do
  hdrId <- randomIO
  let query = LBS.toStrict $ serializeQuery hdrId domainName rt

  runUDPClient ipAddress "53" $ \s sAddr -> do
    sendAllTo s query sAddr
    msg <- recv s 1024
    either (throwIO . DNSParsingError) pure $ parseOnly parsePacket msg

resolve :: DNSHostName -> DNSRecordType -> IO IPv4Address
resolve = go rootServer
  where
    go ip dn rt = do
      putStrLn $ "Querying " <> ip <> " for " <> show dn
      response <- sendQuery ip dn rt
      case (findAnswer response, findNameServerIp response, findNameServer response) of
        (Nothing, Nothing, Nothing) -> throwIO DNSResolveError
        (Just ip', _, _) -> pure ip'
        (_, Just nsIp, _) -> go (show nsIp) dn rt
        (_, _, Just ns) -> do
          ip' <- resolve ns A
          go (show ip') dn rt

findAnswer :: DNSPacket -> Maybe IPv4Address
findAnswer =
  (toIPV4 . recordData =<<) . find ((== A) . recordType) . answers

findNameServerIp :: DNSPacket -> Maybe IPv4Address
findNameServerIp =
  (toIPV4 . recordData =<<) . find ((== A) . recordType) . additionals

toIPV4 :: DNSDataType -> Maybe IPv4Address
toIPV4 (IPv4 address) = Just address
toIPV4 _ = Nothing

findNameServer :: DNSPacket -> Maybe DNSHostName
findNameServer =
  (toNS . recordData =<<) . find ((== NS) . recordType) . authorities
  where
    toNS (NameServer ns) = Just ns
    toNS _ = Nothing
