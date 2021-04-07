module TwentySixteen.Day11 (
  part1,
  part2
) where

import Protolude
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Set as Set

import Utils

-- This solution actually makes all of the moves.


part1 :: IO Int
part1 = pure 42

part2 :: IO Int
part2 = pure 42

data BoardState = BS {bsFloors :: Map Int [Thing], elevator:: Int}
  deriving (Show, Eq, Ord)

floorFromState ::
  BoardState
  -> Int
  -> [Thing]
floorFromState s floorNum = bsFloors s M.! floorNum

terminalState ::
  BoardState
  -> Bool
terminalState state =
  (elevator state == 3) &&
  null (bsFloors state M.! 0) &&
  null (bsFloors state M.! 1) &&
  null (bsFloors state M.! 2)

type StateGraph = Map BoardState [BoardState]

newtype Floor = Floor [Thing]
  deriving (Show, Eq)

-- How far does each element need to move to reach the top floor?
type Scores = Map Thing Int

data Thing
  = Generator Text
  | Chip Text
  deriving (Show, Eq, Ord)

isChip (Chip _) = True
isChip _ = False

isGenerator (Generator _) = True
isGenerator _ = False

matchedPair ::
  Thing
  -> Thing
  -> Bool
matchedPair (Generator a) (Chip b) = a == b
matchedPair (Chip a) (Generator b) = a == b
matchedPair _ _ = False


-- a BFS search through a move graph
findShortestPath ::
  StateGraph
  -> BoardState
  -> Int
findShortestPath graph initialState =
  go (S.singleton (initialState, 0)) Set.empty
  where
    go ((s, dist) S.:<| frontier) seen
      | terminalState s = dist
      | otherwise = let
        edges = graph M.! s
        frontierStates = fst <$> frontier
        novelStates = filter (`Set.notMember` seen) edges
        withDistances = (, dist +1) <$> novelStates
        f = if elevator s == 3
            then traceShow (s, dist) go
            else go
        in go (frontier S.>< S.fromList withDistances) (Set.insert s seen)

-- The current failure suggests that the graph is incomplete? How many nodes should/could it have?

createMoveGraph ::
  BoardState
  -> Int
createMoveGraph initialState =
  go (S.singleton (initialState, 0)) Set.empty
  where
    go S.Empty stateGraph = -1
    go ((s, dist)  S.:<| frontierStates) seen
      | terminalState s = dist
      | otherwise = let
          focusedFloor = floorFromState s $ elevator s
          maxFloor = 3
          minFloor = 0

          (upwardPairs, upwardSingle) =
            if elevator s < maxFloor
            then movesFromFloor focusedFloor (floorFromState s (elevator s + 1))
            else ([], [])
          upwardStates =
            if elevator s < maxFloor
            then updateState 1 s <$> moveList upwardPairs (if L.null upwardPairs then upwardSingle else []) -- Only move pairs up if possible
            else []

          emptyBelow = all identity [ L.null (floorFromState s n) | n <- [0 .. max (elevator s - 1) 0] ]
          (downwardPairs, downwardSingle) =
            if elevator s > minFloor && not emptyBelow
            then movesFromFloor focusedFloor (floorFromState s (elevator s - 1))
            else ([], [])
          downwardStates =
            if elevator s > minFloor && not emptyBelow
            then updateState (-1) s <$> moveList (if L.null downwardSingle then downwardPairs else []) downwardSingle -- Only move a single items down if possible
            else []

          novelStates = (, dist + 1) <$> filter (\k -> not $ k `Set.member` seen) (upwardStates <> downwardStates)
          seen' = foldl (flip Set.insert) seen (fst <$> novelStates)

          updatedFrontier = frontierStates S.>< S.fromList novelStates
          in go updatedFrontier seen'

addStateToGraph ::
  BoardState -- Origin
  -> StateGraph -- Graph
  -> BoardState -- Destination
  -> StateGraph
addStateToGraph origin graph destination = let
  graph' = M.insertWith (\new existing -> existing <> new) destination [origin] graph -- Using an empty list rather than the origin keeps the graph directed. Should this be a directed graph?
  in M.insertWith (\new existing -> existing <> new) origin [destination] graph'


updateState ::
  Int -- Elevator change
  -> BoardState -- starting state
  -> [Thing] -- move
  -> BoardState
updateState elevatorChange startingState move =
  BS {
    elevator = targetNum
  , bsFloors = updatedFloors
  }
  where
    floorNum = elevator startingState
    targetNum =  floorNum + elevatorChange
    startingFloor = (bsFloors startingState) M.! floorNum
    targetFloor = (bsFloors startingState) M.! targetNum
    (fromFloor, toFloor) = doMove move startingFloor targetFloor

    updatedFloors = M.insert targetNum toFloor $
      M.insert floorNum fromFloor (bsFloors startingState)

doMove ::
  [Thing]
  -> [Thing]
  -> [Thing]
  -> ([Thing], [Thing])
doMove t fromFloor toFloor =
  (filter (\a -> not $ a `elem` t) fromFloor, t<>toFloor)

-- Determine which moves can be made between two floors, making sure both floors are in a valid state afterwards
movesFromFloor ::
  [Thing]
  -> [Thing]
  -> ([(Thing, Thing)], [Thing])
movesFromFloor fromFloor toFloor =
  (pairedMoves, singleMoves)
  where
    singleMoves = filter (\thing -> checkFloors (doMove [thing] fromFloor toFloor)) fromFloor
    pairedMoves = filter (\(a,b) -> checkFloors (doMove [a,b] fromFloor toFloor)) thingPairs

    checkFloors (from, to) = floorIsSafe from && floorIsSafe to

    thingPairs = pairs fromFloor

floorIsSafe ::
  [Thing]
  -> Bool
floorIsSafe things =
    all (\t -> any (matchedPair t) things) (filter isChip things) || -- All chips are matched to a generator
    all isChip things || -- The floor is only chips
    all isGenerator things -- The floor is only generators


moveList tupled single = ((\(a,b) -> [a, b]) <$> tupled) <> ((: []) <$> single)

problem1Input =
  BS {
    elevator = 0
  , bsFloors = M.fromList [
    (0, [
          Chip "a"
        , Generator "a"
        , Generator "b"
        , Generator "c"
      ]
    ),
    (1, [
        Chip "c"
      , Chip "b"
      ]
    ),
    (2, [
        Generator "d"
      , Chip "d"
      , Generator "e"
      , Chip "e"
    ]),
    (3, [])
    ]
  }

testInput =
  BS {
    elevator = 0,
    bsFloors = M.fromList [
      (0, [Chip "h", Chip "l"])
    , (1, [Generator "h"])
    , (2, [Generator "l"])
    , (3, [])
    ]
  }

terminalInputState =
  BS {elevator = 3,
    bsFloors = M.fromList [(0, []), (1, []), (2, []), (3, concatMap (\c -> [Chip c, Generator c]) ["a", "b", "c", "d", "e"] )]
    }
