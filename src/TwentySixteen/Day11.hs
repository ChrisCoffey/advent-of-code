module TwentySixteen.Day11 (
  part1,
  part2
) where

import Protolude
import qualified Data.Map as M
import qualified Data.List as L

import Utils

-- This solution actually makes all of the moves.


part1 :: IO Int
part1 = pure 42

part2 :: IO Int
part2 = pure 42

data Elevator = Elevator {eMoves:: Int, eFloor:: Int}
  deriving (Show, Eq)

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


solve ::
  (Elevator, [Floor])
  -> (Elevator, [Floor])
solve input = let
  output = uncurry makeAMove input
  in if input == output
     then output
     else solve output

-- No branching yet
--
-- The guiding principle is to move all things from the lower floors to higher floors as quickly as possible
-- Because the elevator can only move one stop at a time, and the state must always be stable, it stands that
-- there is nothing to gain by jumping ahead. This necessitates the elevator moving backwards with nothing onboard,
-- which increases the move count unnecessarily. Backwards moves should be minimized, as should moves without any
-- cargo.
--
-- In fact, the goal should be to maximize the amount of cargo moved upwards on each turn
makeAMove ::
  Elevator
  -> [Floor] -- Board state
  -> (Elevator, [Floor]) -- Updated scores, elevator, and board state
makeAMove elevator buildingState
  | notEmpty lowerMoves = (Elevator {eMoves = eMoves elevator + 1, eFloor = focusedFloor - 1}, buildingState)
  | notEmpty paired && not topFloor = let -- This is a situation where a zipper would be quite useful
    -- update the two floors to reflect this move. Such a pain in the ass with persistent structures
    (a,b) = L.head paired
    (fl', fNext') = doMove a fl fNext
    (fl'', fNext'') = doMove b fl' fNext'
    buildingState' = (take focusedFloor buildingState)<>[fl'', fNext'']<>(drop (focusedFloor + 2) buildingState)
    in (Elevator {eMoves = eMoves elevator + 1, eFloor = focusedFloor + 1}, buildingState')
  | notEmpty single && not topFloor = let
    a = L.head single
    (fl', fNext') = doMove a fl fNext
    buildingState' = (take focusedFloor buildingState)<>[fl', fNext']<>(drop (focusedFloor + 2) buildingState)
    in (Elevator {eMoves = eMoves elevator + 1, eFloor = focusedFloor + 1}, buildingState')
  | otherwise = (elevator, buildingState)
  where
    focusedFloor = eFloor elevator
    topFloor = focusedFloor == length buildingState - 1
    fl = buildingState L.!! focusedFloor
    fNext = if topFloor
            then fl
            else buildingState L.!! (focusedFloor + 1)
    (paired, single) = movesFromFloor fl fNext

    -- Evaluate speculative moves from lower floors. There can, in theory, be cargo on floors arbitrarily low, so
    -- this needs to account for that possibility.
    -- :: [(moves, (from, to))]
    lowerMoves = [(moves, floorPairs) |
      floorPairs <- floorMovePairs,
      let moves = uncurry movesFromFloor floorPairs,
      notEmpty (fst moves) || notEmpty (snd  moves)
      ]
    floorMovePairs = floorsToCurrent `zip` L.tail floorsToCurrent
    floorsToCurrent = take (1 + focusedFloor) buildingState

doMove ::
  Thing
  -> Floor
  -> Floor
  -> (Floor, Floor)
doMove t (Floor fromFloor) (Floor toFloor) =
  (Floor $ filter (/= t) fromFloor, Floor (t:toFloor))


-- Determine which moves can be made between two floors
movesFromFloor ::
  Floor
  -> Floor
  -> ([(Thing, Thing)], [Thing])
movesFromFloor (Floor fromFloor) (Floor toFloor) =
  (pairedMoves, singleMoves)
  where
    singleMoves = filter (`canMove` Floor toFloor) fromFloor
    pairedMoves = filter (`canMovePair` Floor toFloor) thingPairs

    thingPairs = pairs fromFloor

-- A move is safe under the following conditions:
-- 1) The item being moved is a chip and the floor contains only chips
-- 2) The item being moved is a generator and the floor contains only generators
-- 3) The item being moved is a chip and its generator is on the target floor
canMove ::
  Thing
  -> Floor
  -> Bool
canMove thing (Floor things) =
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
  -> Floor
  -> Bool
canMovePair (a,b) (Floor things) =
  (matchedPair a b && (balanced || all isGenerator things) ) ||
  (canMove a (Floor (b:things)) && canMove b (Floor (a:things)))
  where
    balanced = all (\t -> any (matchedPair t) things) things

problem1Input = [
    Floor [
        Chip "thulium"
      , Generator "thulium"
      , Generator "stronium"
      , Generator "plutonium"
    ]
   ,Floor [
        Chip "plutonium"
      , Chip "stronium"
   ]
   ,Floor [
        Generator "promethium"
      , Chip "promethium"
      , Generator "ruthenium"
      , Chip "ruthenium"
    ]
   ,Floor []
  ]

computeScores ::
  [Floor]
  -> Scores
computeScores floors =
  foldl distance (M.empty, distanceFromTop) floors & fst
  where
    distanceFromTop = length floors -1

    distance :: (Scores, Int) -> Floor -> (Scores, Int)
    distance (scores, remaining) (Floor fl) = (foldl (\m k -> M.insert k remaining m) scores fl, remaining - 1)

