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

type Frontier = Seq Step

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
    Nothing ->
      []
    Just cell ->
      explore maze (Seq.singleton (Step (0, cell))) S.empty <*> [cell]

explore ::
  Maze
  -> Frontier
  -> S.Set Cell
  -> [Cell -> Edge]
explore maze frontier seenCache =
  case Seq.viewl frontier of
    ((Step (dist, cell)) Seq.:< rest) | S.member cell seenCache ->
      explore maze rest seenCache

    ((Step (dist, cell)) Seq.:< rest) ->
      let nextCells = cellsToVisit maze cell
          frontier' = rest Seq.>< Seq.fromList ((\c -> Step (dist+1, c)) <$> nextCells)
          seenCache' = S.insert cell seenCache
      in
        if pointOfInterest maze cell
        then (\start -> Edge {start = start, end = cell, weight = dist}) : explore maze frontier' seenCache'
        else explore maze frontier' seenCache'

    Seq.EmptyL ->
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


