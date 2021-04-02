module TwentySixteen.Day11 (
  part1,
  part2
) where

import Protolude
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Sequence as S

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

createMoveGraph ::
  BoardState
  -> StateGraph
createMoveGraph initialState =
  go (S.singleton initialState) M.empty
  where
    go frontierStates stateGraph
      | S.null frontierStates = stateGraph
      | otherwise = let
        (s S.:< remainingStates) = S.viewl frontierStates
        focusedFloor = floorFromState s $ elevator s
        maxFloor = 3
        minFloor = 0

        moveList tupled single = (toList <$> tupled) <> ((: []) <$> single)

        (upwardPairs, upwardSingle) =
          if elevator s < maxFloor
          then movesFromFloor focusedFloor (floorFromState s (elevator s + 1))
          else ([], [])
        upwardStates =
          if elevator s < maxFloor
          then updateState 1 s <$> moveList upwardPairs upwardSingle
          else []

        (downwardPairs, downwardSingle) =
          if elevator s > minFloor
          then movesFromFloor focusedFloor (floorFromState s (elevator s - 1))
          else ([], [])
        downwardStates =
          if elevator s > minFloor
          then updateState (-1) s <$> moveList downwardPairs downwardSingle
          else []

        novelStates = filter (\k -> not $ k `M.member` stateGraph) (upwardStates <> downwardStates)
        stateGraph' = foldl (addStateToGraph s) stateGraph novelStates

        updatedFrontier = remainingStates S.>< S.fromList novelStates
        in go updatedFrontier stateGraph'

addStateToGraph ::
  BoardState -- Origin
  -> StateGraph -- Graph
  -> BoardState -- Destination
  -> StateGraph
addStateToGraph origin graph destination = let
  graph' = M.insertWith (const identity ) destination [] graph
  in M.insertWith (\new existing -> existing <> new) origin [destination] graph'


updateState ::
  Int -- Elevator change
  -> BoardState -- starting state
  -> [Thing] -- move
  -> BoardState
updateState elevatorChange startingState move =
  BS {
    elevator = elevator startingState + elevatorChange
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

-- Determine which moves can be made between two floors
movesFromFloor ::
  [Thing]
  -> [Thing]
  -> ([(Thing, Thing)], [Thing])
movesFromFloor fromFloor toFloor =
  (pairedMoves, singleMoves)
  where
    singleMoves = filter (`canMove` toFloor) fromFloor
    pairedMoves = filter (`canMovePair` toFloor) thingPairs

    thingPairs = pairs fromFloor

-- A move is safe under the following conditions:
-- 1) The item being moved is a chip and the floor contains only chips
-- 2) The item being moved is a generator and the floor contains only generators
-- 3) The item being moved is a chip and its generator is on the target floor
canMove ::
  Thing
  -> [Thing]
  -> Bool
canMove thing things =
  all isChip (thing:things) ||
  all isGenerator (thing:things) ||
  balanced
  where
    balanced = all (\t -> any (matchedPair t) (thing:things)) (thing:things)

-- Moving a pair is safe if:
-- 1) This is a matched pair to protect the chip, and the destination floor won't destroy any chips
-- 2) Each element in the pair can move independently
canMovePair ::
  (Thing, Thing)
  -> [Thing]
  -> Bool
canMovePair (a,b) things =
  (matchedPair a b && (balanced || all isGenerator things) ) ||
  (canMove a (b:things) && canMove b (a:things))
  where
    balanced = all (\t -> any (matchedPair t) things) things

problem1Input =
  BS {
    elevator = 0
  , bsFloors = M.fromList [
    (0, [
          Chip "thulium"
        , Generator "thulium"
        , Generator "stronium"
        , Generator "plutonium"
      ]
    ),
    (1, [
        Chip "plutonium"
      , Chip "stronium"
      ]
    ),
    (2, [
        Generator "promethium"
      , Chip "promethium"
      , Generator "ruthenium"
      , Chip "ruthenium"
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
