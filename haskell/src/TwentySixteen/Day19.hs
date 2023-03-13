module TwentySixteen.Day19 where

import Protolude

import qualified Data.Sequence as Seq



josephus ::
  Int
  -> Int
josephus n = let
  powOf2 = 2 ^ (floor (logBase 2 $ fromIntegral n))
  l = n - powOf2
  in (2 * l ) + 1


part1 = josephus 3017957

part2 ::
  Int
  -> Int
part2 n = stealGifts $ Seq.fromList [1..n]


stealGifts ::
  Seq.Seq Int
  -> Int
stealGifts (a Seq.:<| Seq.Empty) = a
stealGifts xs = stealGifts $ removeElf xs
  where
    removeElf elves = let
      len = Seq.length elves
      oppositeIdx = len `div` 2
      elves' = Seq.deleteAt oppositeIdx elves
      (e Seq.:< rest) = Seq.viewl elves'
      in rest Seq.|> e
