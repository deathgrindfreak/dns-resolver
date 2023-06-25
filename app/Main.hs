{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString.Char8 as BS
import Options.Applicative
import Text.Read (readMaybe)

import DNS (sendQuery)

data RunQueryErr = BadRecordTypeError
  deriving Show

instance Exception RunQueryErr

data Args = Args
  { domain :: String
  , recordType :: String
  }

parseArgs :: ParserInfo Args
parseArgs =
  info
    (args <**> helper)
    ( fullDesc
        <> header "dns-resolver - Simple DNS resolver"
    )
  where
    args =
      Args
        <$> argument str (metavar "DOMAIN_NAME")
        <*> argument str (metavar "RECORD_TYPE")

main :: IO ()
main = do
  Args {domain, recordType} <- execParser parseArgs
  case readMaybe recordType of
    Nothing -> throwIO BadRecordTypeError
    Just rt -> sendQuery "8.8.8.8" (BS.pack domain) rt
