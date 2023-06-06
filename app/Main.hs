{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Options.Applicative

import DNS (runQuery)

data Args = Args
  { domain :: String
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

main :: IO ()
main = do
  Args {domain} <- execParser parseArgs
  runQuery (BS.pack domain)
