module Main (main) where

import DNS

main :: IO ()
main = runQuery "example.com"
