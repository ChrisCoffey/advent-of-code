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
  Scanner
  -> Int
layerScore scanner
  | (layer scanner) `mod` ((depth scanner -1) *2) == 0 = layer scanner * depth scanner
  | otherwise = 0


loadInput ::
  FilePath
  -> IO [Scanner]
loadInput path = do
  raw <- readFile path
  let lns = lines raw
  pure $ map parseScanner lns
  where
    parseScanner = (\[l,d] -> Scanner {depth = d, layer = l}). map parseInteger . splitOn ": "


