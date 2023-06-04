module Main (main) where

import Dns

main :: IO ()
main = runQuery "example.com"
