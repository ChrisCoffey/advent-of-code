module TwentySeventeen.Day10 where

import Protolude
import qualified Data.Text as T
import Data.Array.IO
import Data.Maybe (fromJust)
import Data.Char (ord)
import Numeric (showHex)

newtype CurrentPosition = Pos Int
  deriving (Show)
newtype SkipSize = Skip Int
  deriving (Show)


type KnotList = IOArray Int Int

knotHash ::
  [Int]
  -> KnotList
  -> CurrentPosition
  -> SkipSize
  -> IO (KnotList, CurrentPosition, SkipSize)
knotHash [] knotted pos skip  = pure (knotted, pos, skip)
knotHash (jumpDist:rest) knotted (Pos i) (Skip n) = do
  (_, upper) <- getBounds knotted
  let len = upper + 1
  let indices = [a `mod` len | a <- [i..(i+(jumpDist-1))] ]
  values <- traverse (readArray knotted) indices
  let flipped = zip indices $ reverse values
  traverse_ (\(idx, v) -> writeArray knotted idx v) flipped
  xs <- getElems knotted

  let nextPosition = (i + jumpDist + n) `mod` len
  knotHash rest knotted (Pos nextPosition) (Skip (n + 1))

inGroupsOf ::
  Int
  -> [a]
  -> [[a]]
inGroupsOf _ [] = []
inGroupsOf n xs = let
  chunk = take n xs
  in chunk: inGroupsOf n (drop n xs)



initialArray :: IO KnotList
initialArray = newListArray (0,255) [0..255]

testArray :: IO KnotList
testArray = newListArray (0,4) [0..4]

part1 :: IO [Int]
part1 = do
  arr <- initialArray
  knotHash [97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190] arr (Pos 0) (Skip 0)
  getElems arr


part2 ::
  FilePath
  -> IO Text
part2 fileName = do
  rawInput <- T.strip <$> readFile fileName
  arr <- initialArray
  let input = ord <$> T.unpack rawInput
      withSuffix = input <> [17, 31, 73, 47, 23]
  foldlM (\(arr, p, s) _ -> knotHash withSuffix arr p s) (arr, Pos 0, Skip 0) [1..64]
  vals <- getElems arr
  let chunks = inGroupsOf 16 vals
      reduced = map (\(a:chunk) -> foldl xor a chunk) chunks
      stringified = concat $ map hexify reduced
  pure $ T.pack stringified
  where
    hexify n
      | n < 16    = '0': showHex n ""
      | otherwise = showHex n ""
