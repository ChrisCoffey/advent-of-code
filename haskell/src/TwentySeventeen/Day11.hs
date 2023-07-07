module TwentySeventeen.Day11 where

import Protolude
import Text.Megaparsec (parse)
import Data.Text (splitOn, strip)

newtype HexPoint = Point (Int, Int, Int)
  deriving (Eq, Ord, Show)

--
--      _______
--    /   x
--   / y
--  |
--  |
--   \ z
--    \_______

move ::
  Text
  -> HexPoint
  -> HexPoint
move "s" (Point (x,y,z)) = Point (x, y+1, z-1)
move "n" (Point (x,y,z)) = Point (x, y-1, z+1)
move "se" (Point (x,y,z)) = Point (x+1, y, z-1)
move "ne" (Point (x,y,z)) = Point (x+1, y-1, z)
move "sw" (Point (x,y,z)) = Point (x-1, y+1, z)
move "nw" (Point (x,y,z)) = Point (x-1, y, z+1)

distance ::
  HexPoint
  -> HexPoint
  -> Int
distance (Point (x,y,z)) (Point (x', y', z')) =
  maximum [abs (x-x') , abs (y-y') , abs(z-z')]

parseInput ::
  FilePath
  -> IO [Text]
parseInput path = do
  raw <- strip <$> readFile path
  let commands = splitOn "," raw
  pure commands

part1 =  fmap (distance start . explore) . parseInput
  where
    explore = foldl (\from cmd -> move cmd from) start
    start = Point (0, 0, 0)


part2 path = do
  input <- parseInput path
  let sitesVisited = scanl (\from cmd -> move cmd from) start input
      distances = fmap (distance start) sitesVisited
  pure $ maximum distances
  where
    start = Point (0, 0, 0)


