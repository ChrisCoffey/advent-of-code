module TwentySixteen.Day14 where

import Protolude

import qualified Crypto.Hash as Cr
import qualified Data.Text as T

type Key = (Text, Int)
type PotentialKey = (Key, Text)
type ValidationHash = (Key, [Text])

generateKeys ::
  Text
  -> [Key]
generateKeys salt = take 64 . fmap fst $ filter (`isValidHash` validationHahes) possibleKeys
  where
    hashStream = zip (hashes salt) [0..]
    possibleKeys = (\(k, x:rest) -> (k, x)) <$> filterRunsOfLength 3 hashStream
    validationHahes = filterRunsOfLength 5 hashStream

generateStretchedKeys ::
  Text
  -> [Key]
generateStretchedKeys salt = take 64 . fmap fst $ filter (`isValidHash` validationHahes) possibleKeys
  where
    hashStream = zip (stretchHashes $ hashes salt) [0..]
    possibleKeys = (\(k, x:rest) -> (k, x)) <$> filterRunsOfLength 3 hashStream
    validationHahes = filterRunsOfLength 5 hashStream

filterRunsOfLength sequenceLength (x@(k, _):rest) = let
  grps = mapMaybe head . filter ((>= sequenceLength) . length) . group $ T.chunksOf 1 k
  in if null grps
      then filterRunsOfLength sequenceLength rest
      else (x, grps): filterRunsOfLength sequenceLength rest

isValidHash ::
  PotentialKey
  -> [ValidationHash]
  -> Bool
isValidHash potentialKey@((_, idx), trips) (((_, vIdx), validationRuns):rest)
  | vIdx > idx + 1000 = False
  | vIdx <= idx = isValidHash potentialKey rest
  | otherwise = elem trips validationRuns || isValidHash potentialKey rest


hashes ::
  Text
  -> [Text]
hashes salt =  md5 . (salt <>) . show <$> [0..]

md5 :: Text -> Text
md5 = decodeUtf8 . show . Cr.hashWith Cr.MD5 . encodeUtf8

stretchHashes ::
  [Text]
  -> [Text]
stretchHashes hsx = (\x -> foldl (\a _ -> md5 a) x [1..2016]) <$> hsx
