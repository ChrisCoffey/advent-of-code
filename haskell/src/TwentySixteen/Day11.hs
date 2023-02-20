module TwentySixteen.Day11 (
) where

import Protolude
import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.IntPSQ as PSQ
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Utils
import GHC.Natural (Natural)


testRun :: Natural
testRun =
  breadthFirstSearch
    solution
    Set.empty
    (Seq.singleton $ Move {node = testInput, depth = 0} )
    -- (PSQ.singleton 0 0 (Move {node = testInput, depth = 0}) )
  where
    solution = Building {
      groundF = emptyFloor,
      secondF = emptyFloor,
      thirdF = emptyFloor,
      fourthF = Floor ["H", "L"] ["H", "L"]
      }

    testInput = Node (Building {
        groundF = Floor ["H", "L"] [] ,
        secondF = Floor [] ["H"],
        thirdF = Floor [] ["L"],
        fourthF = Floor [] []
        }, FirstF)

part1 :: Natural
part1 =
  breadthFirstSearch
    solution
    Set.empty
    (Seq.singleton $ Move {node = part1Input, depth = 0} )
    -- (PSQ.singleton 0 0 (Move {node = part1Input, depth = 0}) )
  where
    solution = Building {
      groundF = emptyFloor,
      secondF = emptyFloor,
      thirdF = emptyFloor,
      fourthF = Floor ["T", "P", "S", "Pr", "R"] ["T", "P", "S", "Pr", "R"]
      }

    part1Input = Node (Building {
        groundF = Floor ["T"] ["T", "P", "S"] ,
        secondF = Floor ["P", "S"] [],
        thirdF = Floor ["Pr", "R"] ["Pr", "R"],
        fourthF = Floor [] []
        }, FirstF)

part2 :: Natural
part2 =
  breadthFirstSearch
    solution
    Set.empty
    (Seq.singleton $ Move {node = part2Input, depth = 0} )
    --(PSQ.singleton 0 0 (Move {node = part1Input, depth = 0}) )
  where
    solution = Building {
      groundF = emptyFloor,
      secondF = emptyFloor,
      thirdF = emptyFloor,
      fourthF = Floor ["T", "P", "S", "Pr", "R", "E", "D"] ["T", "P", "S", "Pr", "R", "E", "D"]
      }

    part2Input = Node (Building {
        groundF = Floor ["T", "E", "D"] ["T", "P", "S", "E", "D"] ,
        secondF = Floor ["P", "S"] [],
        thirdF = Floor ["Pr", "R"] ["Pr", "R"],
        fourthF = Floor [] []
        }, FirstF)



newtype Generator = Generator Text
  deriving (Eq, Ord, Show)

newtype Chip = Chip Text
  deriving (Eq, Ord, Show)

compChipGen :: Chip -> Generator -> Bool
compChipGen (Chip t) (Generator s) = t == s

data Floor = Floor { chips :: [Text], generators :: [Text] }
  deriving (Eq, Ord, Show)

emptyFloor = Floor [] []

data Building = Building {
    groundF :: Floor
  , secondF :: Floor
  , thirdF :: Floor
  , fourthF :: Floor
  }
  deriving (Eq, Ord, Show)


-- Transform a Building into an somewhat ugly but legible text representation
prettyPrint :: Building -> Text
prettyPrint building =
  T.intercalate ("\n" :: Text) [
    "4: " <> prettyFloor (fourthF building)
  , "3: " <> prettyFloor (thirdF building)
  , "2: " <> prettyFloor (secondF building)
  , "1: " <> prettyFloor (groundF building)
  ]
  where
    prettyFloor :: Floor -> Text
    prettyFloor (Floor chips generators) = T.intercalate ("  " :: Text) $ map prettyGen (sort generators) <> map prettyChip (sort chips)

    prettyGen t = "G-" <> t
    prettyChip t = "C-" <> t


-- A move is a (Building, Elevator) pair

-- From the given move, push each valid move onto a queue
-- Then, pop each move off the queue and evaluate it
-- If the move is invalid, continue on
-- If the move is valid, check if the move is a solution
-- Return to the first step and continue

data ElevatorPosition = FirstF | SecondF | ThirdF | FourthF deriving (Eq, Ord, Show)
newtype Node = Node (Building, ElevatorPosition)
  deriving (Eq, Show)

instance Ord Node where
  l <= r = nodeScore l <= nodeScore r

data Move = Move {
    node :: Node
  , depth :: Natural
  }
  deriving (Eq, Ord, Show)

instance Hashable Move where
  hashWithSalt _ move = let
    (Node (building, floor)) = node move
    hashContent = show (depth move) <> show floor <> prettyPrint building
    in hash hashContent

breadthFirstSearch ::
  Building ->
  Set.Set Text ->
  Seq.Seq Move ->
  -- PSQ.IntPSQ Int Move ->
  Natural
breadthFirstSearch finalState = performSearch
  where
    performSearch seen proposedMoves
      | Set.member (keyHash $ node move) seen = performSearch seen moveQueue
      | solution (node move) = trace ("Solved"::Text) $ depth move
      | otherwise = let
          d = depth move
          proposedMoves = (\n -> Move n (d+1)) <$> (filter (not . invalid updatedPositions) . validNodes $ node move)
          -- nextMoves = foldl' (\acc m -> PSQ.insert (keyHash $ node m) (moveScore m) m acc) moveQueue proposedMoves
          moveQueue' = foldl' (\acc m -> acc Seq.|> m) moveQueue proposedMoves
          in performSearch updatedPositions  moveQueue' -- (trace (nodeDebugMessage $ node move) moveQueue')
          where
          (move Seq.:< moveQueue) = Seq.viewl proposedMoves
          updatedPositions = Set.insert (keyHash $ node move) seen

    -- Generate all one or two item permutations from the current floor
    validNodes (Node (building, FirstF)) =
      atFloor SecondF . applyMove building FirstF SecondF <$>  allMoveSets  (groundF building) (secondF building)
    validNodes (Node (building, SecondF)) = let
      movesDown = atFloor FirstF . applyMove building SecondF FirstF <$> allMoveSets (secondF building) (groundF building)
      movesUp = atFloor ThirdF . applyMove building SecondF ThirdF <$> allMoveSets (secondF building) (thirdF building)
      in movesDown <> movesUp
    validNodes (Node (building, ThirdF)) = let
      movesDown = atFloor SecondF . applyMove building ThirdF SecondF <$> allMoveSets (thirdF building) (secondF building)
      movesUp = atFloor FourthF . applyMove building ThirdF FourthF <$> allMoveSets (thirdF building) (fourthF building)
      in movesDown <> movesUp
    validNodes (Node (building, FourthF)) =
       atFloor ThirdF . applyMove building FourthF ThirdF <$> allMoveSets (fourthF building) (thirdF building)

    nodeDebugMessage n@(Node (b, f)) =
      "----\n" <>
      keyHash n <>
      "\n\n" <>
      prettyPrint b



    -- This function needs to not only render the new floor, but also the old floor without the moved values
    -- then apply those changes to the building itself
    allMoveSets :: Floor -> Floor -> [(Floor, [Text], [Text])]
    allMoveSets (Floor chips gens) (Floor destChips destGens) = let
      oneChip = (\c -> (Floor (c <> destChips) destGens, c, [])) <$> chips `choose` 1
      twoeChips = (\cs -> (Floor (cs <> destChips) destGens, cs, [])) <$>  chips `choose` 2
      oneGen = (\g -> (Floor destChips (g <> destGens), [], g)) <$> gens`choose` 1
      twoGens = (\gs -> (Floor destChips (gs <> destGens), [], gs)) <$> gens`choose` 2
      mixed = (\(c,g) -> (Floor (c: destChips) (g:destGens), [c], [g])) <$> pairwisePerms chips gens
      in  oneChip <> twoeChips <> oneGen <> twoGens <> mixed

    applyMove :: Building -> ElevatorPosition -> ElevatorPosition -> (Floor, [Text], [Text]) -> Building
    applyMove building FirstF SecondF (f, removeChips, removeGens) =
      building { secondF = f, groundF = removeFromFloor (groundF building) removeChips removeGens }
    applyMove building SecondF FirstF (f, removeChips, removeGens) =
      building {  groundF = f, secondF = removeFromFloor (secondF building) removeChips removeGens }
    applyMove building SecondF ThirdF (f, removeChips, removeGens) =
      building { thirdF = f, secondF = removeFromFloor (secondF building) removeChips removeGens }
    applyMove building ThirdF SecondF (f, removeChips, removeGens) =
      building { secondF = f, thirdF = removeFromFloor (thirdF building) removeChips removeGens }
    applyMove building ThirdF FourthF (f, removeChips, removeGens) =
      building { fourthF = f, thirdF = removeFromFloor (thirdF building) removeChips removeGens }
    applyMove building FourthF ThirdF (f, removeChips, removeGens) =
      building { thirdF = f, fourthF = removeFromFloor (fourthF building) removeChips removeGens }

    removeFromFloor from removeChips removeGens = let
      fromChips = chips from L.\\ removeChips
      fromGens = generators from L.\\ removeGens
      in  Floor fromChips fromGens

    atFloor :: ElevatorPosition -> Building -> Node
    atFloor f b = Node (b, f)


    -- The validity of a certain building state. If there are no items on the floor the elevator ended up on, or
    -- a chip gets fried, it is an invalid move
    invalid seen node@(Node (building, elevatorLoc)) = Set.member (keyHash node) seen || noItemsOnFloor elevatorLoc || friedChip
      where
        noItemsOnFloor FirstF = emptyFloor $ groundF building
        noItemsOnFloor SecondF = emptyFloor $ secondF building
        noItemsOnFloor ThirdF = emptyFloor $ thirdF building
        noItemsOnFloor FourthF = emptyFloor $ fourthF building

        friedChip = not $ all safeFloor [groundF building, secondF building, thirdF building, fourthF building]

        -- a floor is safe if either: all chips have an associated generator, or: there are no generators
        safeFloor (Floor chips generators) = all (`elem` generators) chips || null generators
        emptyFloor (Floor chips generaors) = null chips && null generaors

    -- make sure these are sorted
    solution (Node (building, FourthF)) = prettyPrint building == prettyPrint finalState
    solution _ = False

-- Ignore the depth, invert priority to appease min heap, see how this pans out...
moveScore :: Move -> Int
moveScore = (* (-1)) . nodeScore . node

nodeScore :: Node -> Int
nodeScore (Node (building, FirstF)) = 1 + buildingScore building
nodeScore (Node (building, SecondF)) = 2 + buildingScore building
nodeScore (Node (building, ThirdF)) = 3 + buildingScore building
nodeScore (Node (building, FourthF)) = 4 + buildingScore building

buildingScore :: Building -> Int
buildingScore building =
  floorScore (groundF building) + ( 2 * floorScore (secondF building)) + (3 * floorScore (thirdF building)) + (4 * floorScore (fourthF building))

floorScore :: Floor -> Int
floorScore (Floor chips generators) = length chips + length generators

keyHash :: Node -> Text
keyHash (Node (building, floor)) = show floor <> "  "<> buildingHash building

buildingHash :: Building -> Text
buildingHash b = let
  elemMap = (floorHash 1 $ groundF b) . (floorHash 2 $ secondF b) . (floorHash 3 $ thirdF b) $ floorHash 4 (fourthF b) M.empty
  in show . sort $ M.elems elemMap

floorHash :: Int -> Floor -> M.Map Text Int -> M.Map Text Int
floorHash floorNum (Floor chips generators) elemKeys = let
  a = foldr (\c acc -> M.insertWith (.|.) c chipKey acc) elemKeys chips
  b = foldr (\g acc -> M.insertWith (.|.) g genKey acc) a generators
  in b
  where
    genKey = floorNum `shiftL` 4
    chipKey = floorNum
