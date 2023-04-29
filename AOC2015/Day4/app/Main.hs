-- Desc: Main module for Advent of Code 2015 day 4.

import Data.ByteString.Lazy.Internal as Data
import Data.Digest.Pure.MD5 (md5)

calcHash string salt = (show (md5 (packChars (string ++ show salt))), salt)

main = do
  let target = "00000"
  let string = "iwrupvqb"
  let hashes = map (calcHash string) [1 ..]
  let hash = head $ filter (\(h, s) -> take (length target) h == target) hashes
  print hash