module TwentySixteen.Day18 where

import Protolude
import qualified Data.Text as T

input :: Text
input = "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."

data FloorTile =
  Trap
  | Safe
  deriving (Eq, Ord, Show)

newtype MappingTripple = Trip (FloorTile, FloorTile, FloorTile)


part1 ::
  Text
  -> Int
  -> Int
part1 input iterations = let
  firstRow = parseInput input
  floorplan = take iterations $ iterate calculateRow firstRow
  in foldl (\acc row -> acc + numSafe row) 0 floorplan

calculateRow ::
  [FloorTile]
  -> [FloorTile]
calculateRow row =
  fmap determineTileType . toMappingTripples $ Safe:row

toMappingTripples ::
  [FloorTile]
  -> [MappingTripple]
toMappingTripples [a,b] = [Trip (a,b,Safe)]
toMappingTripples (a:b:c:rest) = Trip (a,b,c) : toMappingTripples (b:c:rest)

determineTileType ::
  MappingTripple
  -> FloorTile
determineTileType (Trip (Trap, Trap, Safe)) = Trap
determineTileType (Trip (Safe, Trap, Trap)) = Trap
determineTileType (Trip (Trap, Safe, Safe)) = Trap
determineTileType (Trip (Safe, Safe, Trap)) = Trap
determineTileType _ = Safe

numSafe ::
  [FloorTile]
  -> Int
numSafe = length . filter isSafe
  where
    isSafe Trap = False
    isSafe Safe = True


parseInput ::
  Text
  -> [FloorTile]
parseInput str =
  T.foldl (\acc c -> parseFunc c : acc) [] str
  where
    parseFunc '^' = Trap
    parseFunc '.' = Safe
