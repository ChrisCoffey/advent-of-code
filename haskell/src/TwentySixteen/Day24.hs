module TwentySixteen.Day24 where

import Protolude
import qualified Data.OrdPSQ as PSQ
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Text as T

waypoints = ['0', '1', '2', '3', '4', '5', '6', '7']

data Edge = Edge {start:: Cell, end:: Cell, weight :: Int}
  deriving (Eq, Ord, Show)

newtype Graph = Graph (Seq Edge)


type X = Int
type Y = Int
newtype Cell = Cell (Y, X)
  deriving (Eq, Ord, Show)

newtype Maze = Maze (Seq (Seq Char))
  deriving (Eq, Ord, Show)

newtype Step = Step (Int, Cell)
  deriving (Eq, Ord, Show)

type Frontier = PSQ.OrdPSQ Step Int Cell

loadInput ::
  FilePath
  -> IO Maze
loadInput path = do
  raw <- readFile path
  let textLines = lines raw
      charLines = Seq.fromList . T.unpack <$> textLines
      charTable = Seq.fromList charLines
  pure $ Maze charTable

buildGraph ::
  Maze
  -> Graph
buildGraph maze =
  Graph . Seq.fromList $ concatMap (buildEdges maze) waypoints

buildEdges ::
  Maze
  -> Char
  -> [Edge]
buildEdges maze startChar =
  let c = locationInMaze startChar maze
  in case c of
    Nothing -> []
    Just cell -> filter (not . selfEdge) $ explore maze (PSQ.singleton (Step (0, cell)) 0 cell) <*> [cell]
  where
    selfEdge (Edge start end _) = start == end

explore ::
  Maze
  -> Frontier
  -> [Cell -> Edge]
explore maze frontier =
  case PSQ.findMin frontier of
    Just ((Step (dist, c)), p, _) ->
      let nextCells = cellsToVisit maze c
          popped = PSQ.deleteMin frontier
          frontier' = foldl (\acc c' -> PSQ.insert (Step (dist+1, c')) (dist+1) c' acc) popped nextCells
      in
        if pointOfInterest maze c
        then (\start -> Edge {start = start, end = c, weight = dist}) : explore maze frontier'
        else explore maze frontier'

    Nothing ->
      []

pointOfInterest ::
  Maze
  -> Cell
  -> Bool
pointOfInterest (Maze maze) (Cell (y, x)) =
  let c = Seq.index (Seq.index maze y) x
  in c `elem` waypoints

locationInMaze ::
  Char
  -> Maze
  -> Maybe Cell
locationInMaze needle (Maze maze) = do
  rowNum <- Seq.findIndexL (isJust . Seq.elemIndexL needle) maze
  columnNum <- Seq.elemIndexL needle (Seq.index maze rowNum)
  pure $ Cell (rowNum, columnNum)

cellsToVisit ::
  Maze
  -> Cell
  -> [Cell]
cellsToVisit maze cell =
  [ c | c <- neighboringPoints cell, canVisit maze c]

canVisit ::
  Maze
  -> Cell
  -> Bool
canVisit (Maze maze) (Cell (y, x)) = let
  c = (Seq.lookup x) =<< (Seq.lookup y maze)
  in case c of
    Nothing -> False
    Just '#' -> False
    _ -> True

neighboringPoints ::
  Cell
  -> [Cell]
neighboringPoints (Cell (y,x)) = Cell <$> [
             (y-1, x)
  ,(y, x-1) ,        (y, x+1)
  ,          (y+1, x)
  ]


