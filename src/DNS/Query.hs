module DNS.Query (runQuery)
where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Run.UDP (runUDPClient)
import Network.Socket.ByteString (recv, sendAllTo)
import System.Random

import DNS.Decode
import DNS.Encode
import DNS.Model

runQuery :: BS.ByteString -> IO ()
runQuery domainName = do
  hdrId <- randomIO
  let query = LBS.toStrict $ serializeQuery hdrId domainName A

  runUDPClient "8.8.8.8" "53" $ \s sAddr -> do
    sendAllTo s query sAddr
    msg <- recv s 1024
    either error print $ parseOnly parsePacket msg
