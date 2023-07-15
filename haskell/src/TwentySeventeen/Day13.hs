module TwentySeventeen.Day13 where

import Protolude
import GHC.Read (readField)
import Data.Text (splitOn)
import TwentySixteen.Day20 (parseInteger)

data Scanner = Scanner {
  depth:: Int,
  layer :: Int
} deriving (Eq, Ord, Show)

layerScore ::
  Int
  -> Scanner
  -> Int
layerScore delay scanner
  | caught delay scanner = layer scanner * depth scanner
  | otherwise = 0

caught ::
  Int
  -> Scanner
  -> Bool
caught delay scanner =
  (delay + layer scanner) `mod` ((depth scanner -1) *2) == 0

loadInput ::
  FilePath
  -> IO [Scanner]
loadInput path = do
  raw <- readFile path
  let lns = lines raw
  pure $ map parseScanner lns
  where
    parseScanner = (\[l,d] -> Scanner {depth = d, layer = l}). map parseInteger . splitOn ": "

part1 file = sum . map (layerScore 0) <$> loadInput file

part2 file = do
  scanners <- loadInput file
  let t = take 10 [ time | time <- [0..], all not ( caught time <$> scanners)]
  pure t
