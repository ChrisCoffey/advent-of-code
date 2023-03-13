module TwentySixteen.Day20 where

import Protolude

import qualified Data.Text as T
import Text.Read (read)

newtype IPRange = IpRange (Int, Int)
  deriving (Eq, Show, Ord)


part1 ::
  FilePath
  -> IO [IPRange]
part1 path = do
  ranges <- parseInput path
  let sortedRanges = sortOn (\(IpRange (low, high)) -> low) ranges
      initialRange = IpRange (0, 4294967295)
      result = foldl updateRanges [initialRange] sortedRanges
  pure result

part2 ::
  FilePath
  -> IO Int
part2 path = do
  allowedRanges <- part1 path
  pure . sum $ ipsInRange <$> allowedRanges

updateRanges ::
  [IPRange]
  -> IPRange
  -> [IPRange]
updateRanges [] _ = []
updateRanges (allowedRange:rest) blockedRange
  | allowedRange `containedBy` blockedRange = updateRanges rest blockedRange
  | allowedRange `contains` blockedRange = let
    (low, high) = splitRange allowedRange blockedRange
    in low:high:rest
  | allowedRange `overlaps` blockedRange = trimRange allowedRange blockedRange : updateRanges rest blockedRange
  | otherwise = allowedRange : updateRanges rest blockedRange


-- Splits a range in two pieces based on another range contained within it
splitRange ::
  IPRange
  -> IPRange
  -> (IPRange, IPRange)
splitRange (IpRange (allowedLow, allowedHigh)) (IpRange (blockedLow, blockedHigh)) = let
  low = IpRange (allowedLow, blockedLow - 1)
  high = IpRange (blockedHigh + 1, allowedHigh)
  in (low, high)

-- Trims the front or low end of the range, based on overlap
trimRange ::
  IPRange
  -> IPRange
  -> IPRange
trimRange (IpRange (allowedLow, allowedHigh)) (IpRange (blockedLow, blockedHigh))
  | allowedLow >= blockedLow = IpRange (blockedHigh + 1, allowedHigh)
  | allowedHigh <= blockedHigh = IpRange (allowedLow, blockedLow - 1)

containedBy ::
  IPRange
  -> IPRange
  -> Bool
containedBy (IpRange (allowedLow, allowedHigh)) (IpRange (blockedLow, blockedHigh)) =
  allowedLow  >= blockedLow && allowedHigh <= blockedHigh

contains ::
  IPRange
  -> IPRange
  -> Bool
contains (IpRange (allowedLow, allowedHigh)) (IpRange (blockedLow, blockedHigh)) =
  allowedLow < blockedLow && allowedHigh > blockedHigh

overlaps ::
  IPRange
  -> IPRange
  -> Bool
overlaps(IpRange (allowedLow, allowedHigh)) (IpRange (blockedLow, blockedHigh)) =
  (allowedLow >= blockedLow && allowedLow < blockedHigh) ||
  (allowedHigh <= blockedHigh && allowedHigh > blockedLow)

ipsInRange ::
  IPRange
  -> Int
ipsInRange (IpRange (low, high)) = (high - low) + 1

parseInput ::
  FilePath
  -> IO [IPRange]
parseInput path =
 fmap parseLine . lines <$> readFile path

parseLine ::
  Text
  -> IPRange
parseLine line = let
  [low, high] = T.splitOn "-" line
  in IpRange (parseInteger low, parseInteger high)

parseInteger ::
  Text
  -> Int
parseInteger = T.foldl (\acc c -> (acc * 10) + parseChar c) 0
  where
    parseChar '0' = 0
    parseChar '1' = 1
    parseChar '2' = 2
    parseChar '3' = 3
    parseChar '4' = 4
    parseChar '5' = 5
    parseChar '6' = 6
    parseChar '7' = 7
    parseChar '8' = 8
    parseChar '9' = 9
    parseChar _ = 0
