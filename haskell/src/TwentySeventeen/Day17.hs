module TwentySeventeen.Day17 where

import Protolude
import qualified Data.Sequence as S

data CircularBuffer = CB {startingPos :: Int, buffer :: Seq Int}
  deriving (Eq, Ord, Show, NFData, Generic)

input = 303


calcStopIndex ::
  CircularBuffer
  -> Int
calcStopIndex (CB start buf) = let
  rotations = input `mod` S.length buf
  in (rotations + start) `mod` S.length buf

step ::
  Int
  -> CircularBuffer
  -> CircularBuffer
step n cb@(CB start buf) = let
  stop = (calcStopIndex cb) + 1
  buffer' = S.insertAt stop n buf
  in CB stop buffer'

part1 = let
  res = foldl' (flip step) (CB 1 (S.fromList [0])) [1..2017]
  in S.take 2 $ S.drop (startingPos res) (buffer res)


part2 :: IO ()
part2 = let
  stages = scanl' (flip step) (CB 1 (S.fromList [0])) [1..50000000]
  res = zip [1..] stages
  neighbors = (\cb -> (fst cb, S.take 2 . buffer $ snd cb)) <$> res
  pairs = zip (tail neighbors) neighbors
  changes = filter (\(l,r) -> snd l /= snd r) pairs
  in forM_ changes print

tail (x:rest) = rest
