module Data.IPV6 (showIPV6) where

import qualified Data.ByteString as BS

import Numeric
import Data.List (groupBy, intercalate)
import Data.Word (Word16)
import Data.Attoparsec.Helper
import Safe

showIPV6 :: BS.ByteString -> String
showIPV6 =
  intercalate ":"
  . replaceEmptyLeadOrTrail
  . map (headDef "" . map (`showHex` ""))
  . reduceConsecutiveZeros
  . groupOnlyZeros
  . groupWords
  where
    middle = initDef [] . tailDef []

    ensureDouble ("":xs) = "":"":xs
    ensureDouble ls = ls

    replaceEmptyLeadOrTrail [""] = [":", ""]
    replaceEmptyLeadOrTrail ls =
       ensureDouble [headDef "" ls] ++ middle ls ++ ensureDouble [lastDef "" ls]

reduceConsecutiveZeros :: (Num a, Eq a) => [[a]] -> [[a]]
reduceConsecutiveZeros gs =
  let maxZeros =
        maximum $ map (\l -> if head l == 0 then length l else 0) gs

      go _ [] = []
      go True (x:xs) = map (:[]) x ++ go True xs
      go False (x:xs) =
        if head x == 0 && length x == maxZeros
          then [] :  go True xs
          else map (:[]) x ++ go False xs
   in go False gs

groupOnlyZeros :: (Eq a, Num a) => [a] -> [[a]]
groupOnlyZeros = groupBy (\a b -> a == 0 && b == 0)

groupWords :: BS.ByteString -> [Word16]
groupWords = go . BS.unpack
  where
    go [] = []
    go ls = fromIntegral (wordsToBEInt (take 2 ls)) : go (drop 2 ls)

-- test :: BS.ByteString
-- test = BS.pack [0x20, 0x01, 0x0d, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0x42, 0x83, 0x29]

-- test2 :: BS.ByteString
-- test2 = BS.replicate 15 0 <> BS.pack [0x01]

-- test3 :: BS.ByteString
-- test3 = BS.pack [0x1A, 0x1B] <> BS.replicate 13 0 <> BS.pack [0x01]

-- test4 :: BS.ByteString
-- test4 = BS.replicate 16 0

-- test5 :: BS.ByteString
-- test5 = BS.pack [0x01] <> BS.replicate 15 0

-- testAll :: IO ()
-- testAll = mapM_ (print . showIPV6) [test, test2, test3, test4, test5]
