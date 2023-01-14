module TwentySixteen.Day13 where

import Protolude

import qualified Data.Array as A
import qualified Data.Set as S

type Point = (Int, Int)
-- The frontier consists of points and the number of steps taken to that point
type Frontier = [(Point, Int)]

newtype And = And [Bool]

navigateMaze ::
  Point
  -> Int
  -> S.Set Point
  -> Frontier
  -> Int
navigateMaze destination magicNumber visited ((currentPoint, steps):rest)
  | destination == currentPoint = steps
  | otherwise = let
    viableMoves = filter (and . (<*>) [(`S.notMember` visited), openSpace magicNumber] . (:[]) ) $ neighboringPoints currentPoint
    additionToFrontier = (,steps+1) <$> viableMoves
    in navigateMaze destination magicNumber (S.insert currentPoint visited) (rest<>additionToFrontier)

reachableNodes ::
  Int
  -> S.Set Point
  -> Frontier
  -> Int
reachableNodes magicNumber visited ((currentPoint, steps):rest)
  | steps > 50= S.size visited
  | otherwise = let
    viableMoves = filter (and . (<*>) [(`S.notMember` visited), openSpace magicNumber] . (:[]) ) $ neighboringPoints currentPoint
    additionToFrontier = (,steps+1) <$> viableMoves
    in reachableNodes magicNumber (S.insert currentPoint visited) (rest<>additionToFrontier)


openSpace ::
  Int
  -> Point
  -> Bool
openSpace magicNumber (x, y) =
  even . popCount $ (x^2) + (3 * x) + (2 * x * y) + y + (y^2) + magicNumber

neighboringPoints ::
  Point
  -> [Point]
neighboringPoints (x,y) = let
  n = [(x, y-1) | y > 0]
  s = [(x, y+1)]
  e = [(x+1, y)]
  w = [(x-1, y) | x > 0]
  in n <> s <> e <> w
