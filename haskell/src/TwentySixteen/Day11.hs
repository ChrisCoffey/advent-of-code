module TwentySixteen.Day11 (
) where

import Protolude
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Set as Set

import Utils
import GHC.Natural (Natural)

newtype Generator = Generator Text
  deriving (Eq, Ord)

newtype Chip = Chip Text
  deriving (Eq, Ord)

compChipGen :: Chip -> Generator -> Bool
compChipGen (Chip t) (Generator s) = t == s

data Floor = Floor { chips :: [Text], generators :: [Text] }
  deriving (Eq, Ord)

data Building = Building {
    groundF :: Floor
  , secondF :: Floor
  , thirdF :: Floor
  , fourthF :: Floor
  }
  deriving (Eq, Ord)


-- Transform a Building into an somewhat ugly but legible text representation
prettyPrint :: Building -> Text
prettyPrint building =
  T.intercalate ("\n" :: Text) [
    prettyFloor (fourthF building)
  , prettyFloor (thirdF building)
  , prettyFloor (secondF building)
  , prettyFloor (groundF building)
  ]
  where
    prettyFloor :: Floor -> Text
    prettyFloor (Floor chips generators) = T.intercalate ("  " :: Text) $ map prettyGen (sort generators) <> map prettyChip (sort chips)

    prettyGen t = "G " <> t
    prettyChip t = "C " <> t


-- A move is a (Building, Elevator) pair

-- From the given move, push each valid move onto a queue
-- Then, pop each move off the queue and evaluate it
-- If the move is invalid, continue on
-- If the move is valid, check if the move is a solution
-- Return to the first step and continue

data ElevatorPosition = FirstF | SecondF | ThirdF | FourthF deriving (Eq, Ord)
newtype Node = Node (Building, ElevatorPosition)
  deriving (Eq, Ord)

data Move = Move {
    node :: Node
  , depth :: Natural
  }



breadthFirstSearch ::
  Building ->
  Set.Set Node ->
  [Move] ->
  Natural
breadthFirstSearch _ _ [] = 0
breadthFirstSearch finalState seenPositions (move:moves)
  | Set.member (node move) seenPositions = breadthFirstSearch finalState seenPositions moves
  | invalid move = breadthFirstSearch finalState updatedPositions moves
  | solution (node move) = depth move
  | otherwise = let
    d = depth move
    nextMoves = \n -> Move n (d+1) <$> (validNodes $ node move)
    in breadthFirstSearch finalState updatedPositions nextMoves

  where
    -- Generate all one or two item permutations from the current floor
    validNodes (Node (building, FirstF)) = map (atFloor SecondF) $ allMoveSets (secondF building) (groundF building)
    validNodes (Node (building, SecondF)) = let
      movesDown = atFloor FirstF <$> allMoveSets (groundF building) (secondF building)
      movesUp = atFloor ThirdF <$> allMoveSets (thirdF building) (secondF building)
      in movesDown <> movesUp
    validNodes (Node (building, ThirdF)) = let
      movesDown = atFloor SecondF <$> allMoveSets (secondF building) (thirdF building)
      movesUp = atFloor FourthF <$> allMoveSets (fourthF building) (thirdF building)
      in movesDown <> movesUp
    validNodes (Node (building, FourthF)) = map (\b -> Node (b, ThirdF)) $ allMoveSets (thirdF building) (fourthF building)

    allMoveSets (Floor destChips destGens) (Floor chips gens) = let
      oneChip = (\c -> Floor (c <> destChips) destGens) <$> chips `choose` 1
      twoeChips = (\cs -> Floor (cs <> destChips) destGens) <$>  chips `choose` 2
      oneGen = (\g -> Floor destChips (g <> destGens)) <$> gens`choose` 1
      twoGens = (\gs -> Floor destChips (gs <> destGens)) <$> gens`choose` 2
      mixed = (\(c,g) -> Floor (c: destChips) (g:destGens)) <$> pairwisePerms chips gens
      in oneChip <> twoeChips <> oneGen <> twoGens <> mixed

    -- TODO update the floor in the building
    atFloor f b = Node (b, f)

    updatedPositions = Set.insert (node move) seenPositions

    invalid (Move (Node (building, elevatorLoc)) _) = noItemsOnFloor elevatorLoc || friedChip
      where
        noItemsOnFloor FirstF = emptyFloor $ groundF building
        noItemsOnFloor SecondF = emptyFloor $ secondF building
        noItemsOnFloor ThirdF = emptyFloor $ thirdF building
        noItemsOnFloor FourthF = emptyFloor $ fourthF building

        friedChip = not $ all safeFloor [groundF building, secondF building, thirdF building, fourthF building]

        safeFloor (Floor chips generators) = all (`elem` generators) chips || null generators
        emptyFloor (Floor chips generaors) = null chips && null generaors

    -- make sure these are sorted
    solution (Node (building, FourthF)) = prettyPrint building == prettyPrint finalState
    solution _ = False

testInput = Node (Building {
    groundF = Floor ["H", "L"] [] ,
    secondF = Floor [] ["H"],
    thirdF = Floor [] ["G"],
    fourthF = Floor [] []
    }, FirstF)
