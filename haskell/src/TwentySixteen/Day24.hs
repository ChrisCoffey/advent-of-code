module TwentySixteen.Day24 where

import Protolude
import qualified Data.Map as M
import qualified Data.OrdPSQ as PSQ
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import TwentySixteen.Day21 (HashOp(a))
import qualified Data.Foldable as Seq

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

-- Only 5k permutations that start with 0, 40k total
shortestCycle ::
  Maze
  -> Cell
  -> Int
shortestCycle maze startingCell =
  minimum $ catMaybes [ pathDistance path | path <- perms ]
  where
    (Graph edges) = buildGraph maze

    indexedGraph = let
        keyed = groupBy (\l r -> start l == start r) . toList $ Seq.sortOn start edges
        storeOutgoingEdges idx (edge:rest) = M.insert (start edge) (edge:rest) idx
        idx = foldl storeOutgoingEdges M.empty keyed
      in idx

    (Just z) = locationInMaze '0' maze
    -- All paths originating at 0
    perms = filter ( maybe False (== z ) . head) . permutations . catMaybes $ [ locationInMaze  w maze | w <- waypoints]

    pathDistance [final] = do
      edges <- M.lookup final indexedGraph
      edge <- find ((== z) . end) edges
      pure $ weight edge

    pathDistance (cell:next:rest) = do
      edges <- M.lookup cell indexedGraph
      edge <- find ((== next) . end) edges
      w' <- pathDistance (next:rest)
      pure $ (weight edge) + w'



-- Starting from a given cell, build up a list to the next closest edge
-- Repeat until all cells have been added
--
-- Optionally, perform a brute-force serach of the graph since it's so small
greedySolve ::
  Graph
  -> Cell
  -> [Edge]
greedySolve (Graph edges) cell =
  let
    eligibleEdges' = Seq.filter (\edge -> end edge /= cell) edges
    outboundEdges = Seq.filter (\edge -> start edge == cell) eligibleEdges'
    nextEdges = Seq.sortOn weight outboundEdges
    in
      case Seq.viewl nextEdges of
        (nextEdge Seq.:< _) ->
          nextEdge : greedySolve (Graph eligibleEdges') (end nextEdge)
        Seq.EmptyL ->
          []

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
  Graph . Seq.fromList . filter (\e -> start e /= end e) $ concatMap (buildEdges maze) waypoints

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
  (`elem` waypoints) $ Seq.index (Seq.index maze y) x

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


