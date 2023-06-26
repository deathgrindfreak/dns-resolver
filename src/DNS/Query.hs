module DNS.Query (sendQuery)
where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Run.UDP (runUDPClient)
import Network.Socket (HostName)
import Network.Socket.ByteString (recv, sendAllTo)
import System.Random
import Text.Pretty.Simple

import DNS.Decode
import DNS.Encode
import DNS.Model

sendQuery :: HostName -> BS.ByteString -> DNSRequestType -> IO ()
sendQuery ipAddress domainName rt = do
  hdrId <- randomIO
  let query = LBS.toStrict $ serializeQuery hdrId domainName rt

  runUDPClient ipAddress "53" $ \s sAddr -> do
    sendAllTo s query sAddr
    msg <- recv s 1024
    either error pPrint $ parseOnly parsePacket msg
