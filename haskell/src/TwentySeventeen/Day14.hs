module TwentySeventeen.Day14 where

import Protolude
import Data.String (String)
import TwentySeventeen.Day10 (knotHash, KnotList, CurrentPosition(..), SkipSize(..), initialArray, inGroupsOf)
import Data.Array.Base (getElems)
import Data.Array.MArray (readArray, writeArray)
import Data.Array.IO (newListArray)
import qualified Data.Text as T
import Numeric (showHex)
import Data.Hashable (hashed)
import Data.Array.IO.Internals (IOArray(IOArray))

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


data BitType
  = Clustered Int
  | Set
  | Clear
  deriving (Eq, Ord, Show)

type Grid = IOArray Int (IOArray Int BitType)

floodCluster ::
  Int -- cluster number
  -> [(Int, Int)] -- (y,x)
  -> Bool
  -> Grid
  -> IO (Grid, Int)
floodCluster clusterNumber [] found grid = pure (grid, clusterNumber + (if found then 1 else 0))
floodCluster clusterNumber ((y, x):rest) found grid = do
  val <- (`readArray` x) =<< grid `readArray` y
  case val of
    Clear -> floodCluster clusterNumber rest found grid
    Clustered _ -> floodCluster clusterNumber rest found grid
    Set -> do
      row <- grid `readArray` y
      writeArray row x (Clustered clusterNumber)
      let near = neighbors (y,x)
      floodCluster clusterNumber (rest<>near) (found || True) grid

neighbors ::
  (Int, Int)
  -> [(Int, Int)]
neighbors (y,x) = [(y2, x2) |
              (y2, x2) <- [(y, x-1), (y, x+1), (y-1, x), (y+1,x)],
              x2 >= 0,
              y2 >= 0,
              x2 < 128,
              y2 < 128
            ]


part2 :: IO Int
part2 = do
  hashes <- mapM hashedRow [0..127]
  let bitLists = toBits . T.unpack <$> hashes
  gridRows <- traverse gridRow bitLists
  grid <- toMatrix gridRows
  let points = [(y, x) | y <- [0..127], x <- [0..127]]
  (grid, n)<- foldM checkCell (grid, 0) points
  pure n

  where
    gridRow :: [Int] -> IO (IOArray Int BitType)
    gridRow = newListArray (0, 127) . map asBitType

    toMatrix :: [IOArray Int BitType] -> IO Grid
    toMatrix = newListArray (0,127)

    asBitType 0 = Clear
    asBitType 1 = Set

    checkCell (grid, clusterNum) point = floodCluster clusterNum [point] False grid
