module TwentySeventeen.Day10 where

import Protolude
import Data.Array.IO
import Data.Maybe (fromJust)
import TwentySixteen.Day21 (HashOp(a))

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
  -> IO KnotList
knotHash [] knotted _ _ = pure knotted
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


initialArray :: IO KnotList
initialArray = newListArray (0,255) [0..255]

testArray :: IO KnotList
testArray = newListArray (0,4) [0..4]

part1 :: IO [Int]
part1 = do
  arr <- initialArray
  knotHash [97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190] arr (Pos 0) (Skip 0)
  getElems arr

