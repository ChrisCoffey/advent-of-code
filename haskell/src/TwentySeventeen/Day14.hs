module TwentySeventeen.Day14 where

import Protolude
import Data.String (String)
import TwentySeventeen.Day10 (knotHash, KnotList, CurrentPosition(..), SkipSize(..), initialArray, inGroupsOf)
import Data.Array.Base (getElems)
import qualified Data.Text as T
import Numeric (showHex)
import Data.Hashable (hashed)

input :: String
input = "ugkiagan"

hashedRow ::
  Int
  -> IO Text
hashedRow rowNum = do
  arr <- initialArray
  let rowInput = map ord $ input <> "-" <> show rowNum
      inputWithSuffix = rowInput <> [17, 31, 73, 47, 23]
  foldM (\(a, p, s) _ -> knotHash inputWithSuffix arr p s) (arr, Pos 0, Skip 0) [1..64]
  vals <- getElems arr
  let chunks = inGroupsOf 16 vals
      reduced = map (\(a:chunk) -> foldl xor a chunk) chunks
      stringified = concat $ map hexify reduced
  pure $ T.pack stringified
  where
    hexify n
      | n < 16    = '0': showHex n ""
      | otherwise = showHex n ""

toBits ::
  String
  -> [Int]
toBits [] = []
toBits ('0':rest) = 0:0:0:0: toBits rest
toBits ('1':rest) = 0:0:0:1: toBits rest
toBits ('2':rest) = 0:0:1:0: toBits rest
toBits ('3':rest) = 0:0:1:1: toBits rest
toBits ('4':rest) = 0:1:0:0: toBits rest
toBits ('5':rest) = 0:1:0:1: toBits rest
toBits ('6':rest) = 0:1:1:0: toBits rest
toBits ('7':rest) = 0:1:1:1: toBits rest
toBits ('8':rest) = 1:0:0:0: toBits rest
toBits ('9':rest) = 1:0:0:1: toBits rest
toBits ('a':rest) = 1:0:1:0: toBits rest
toBits ('b':rest) = 1:0:1:1: toBits rest
toBits ('c':rest) = 1:1:0:0: toBits rest
toBits ('d':rest) = 1:1:0:1: toBits rest
toBits ('e':rest) = 1:1:1:0: toBits rest
toBits ('f':rest) = 1:1:1:1: toBits rest

part1 :: IO Int
part1 = do
  hashes <- mapM hashedRow [0..127]
  let bitLists = toBits . T.unpack <$> hashes
      bitSums = sum <$> bitLists
  pure $ sum bitSums
