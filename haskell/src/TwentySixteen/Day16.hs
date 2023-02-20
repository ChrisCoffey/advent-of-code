module TwentySixteen.Day16 where

import Protolude
import qualified Data.Sequence as S

fakeData ::
  Int ->
  [Int] ->
  [Int]
fakeData dataLen input
  | length input < dataLen = fakeData dataLen $ generateData input
  | otherwise = take dataLen input

generateData :: [Int] -> [Int]
generateData a = a <> [0] <> flipBits (reverse a)

flipBits :: [Int] -> [Int]
flipBits [] = []
flipBits (1:xs) = 0 : flipBits xs
flipBits (0:xs) = 1 : flipBits xs


checksum ::
  [Int] ->
  [Int]
checksum input
  | odd $ length input = input
  | otherwise = checksum $ step input
    where
      step [] = []
      step (0:0:xs) = 1 : step xs
      step (1:1:xs) = 1 : step xs
      step (_:_:xs) = 0 : step xs


test = let
  a = fakeData 20 [1, 0, 0, 0, 0]
  in checksum $ trace ((show a) :: Text) a

part1 = checksum $ fakeData 272 [1,1,1,0,1,0,0,0,1,1,0,0,1,0,1,0,0]

part2 = checksum $ fakeData 35651584 [1,1,1,0,1,0,0,0,1,1,0,0,1,0,1,0,0]
