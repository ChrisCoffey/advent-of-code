module TwentySeventeen.Day19 where

import Protolude hiding (Down, Right, Left)
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Sequence (unfoldl)

data Point = Point {x:: Int, y:: Int}
  deriving (Eq, Ord, Show)
-- Indexed as (Y, X)
type Row = S.Seq Char
type Cell = Char

-- 0 is the top index, increasing the index moves DOWN the diagram
-- Rows progress l->r from 0 ->N
type Diagram = S.Seq Row

data Direction = Up | Down | Left | Right
  deriving (Eq, Ord, Show)

vertical = '|'
turn = '+'
horizontal = '-'
emptyCell = ' '

-- Take a single step through the diagram, turning as necessary
-- Starts at a given point and takes a step in the indicated direction.
--
-- If the current point is a '+', it searches through to find the turn to
-- follow, checking that the new direction isn't the inverse of the direction
-- it's traveling in
step ::
  Diagram
  -> Point
  -> Direction
  -> Maybe (Point, Direction)
step diagram p@(Point {x, y}) currentDirection = do
  currentCell <- cell diagram p
  if currentCell == turn || currentCell == emptyCell
  then head findNext -- This is Nothing at the very end of the path
  else Just (followDirection, currentDirection)

  where
    findNext =
      case currentDirection of
        Down ->
          exploreAdjacentList [ (moveLeft, Left), (moveRight, Right) ]
        Up ->
          exploreAdjacentList [ (moveLeft, Left), (moveRight, Right) ]
        Left ->
          exploreAdjacentList [ (moveUp, Up), (moveDown, Down) ]
        Right ->
          exploreAdjacentList [ (moveUp, Up), (moveDown, Down) ]

    followDirection =
      case currentDirection of
        Down -> moveDown
        Up -> moveUp
        Left -> moveLeft
        Right -> moveRight


    exploreAdjacentList lst = [(p', d) | (p', d)<- lst, occupied p']

    occupied point =
      maybe False (\c -> emptyCell /= c && inbounds point) $ cell diagram point

    inbounds (Point {x, y}) =
      y >= 0 &&
      y < S.length diagram &&
      x >= 0 &&
      x < maximum (S.length <$> diagram)

    moveDown = Point {x = x, y = y+1}
    moveUp = Point {x = x, y = y - 1}
    moveLeft = Point {x = x-1, y = y}
    moveRight = Point {x = x+1, y = y}

cell ::
  Diagram
  -> Point
  -> Maybe Cell
cell diagram (Point {x, y}) =
  (S.!? x) =<< (diagram S.!? y)

findStart ::
  Diagram
  -> Maybe Point
findStart diagram = do
  firstRow <- diagram S.!? 0
  startingColumn <- S.findIndexL (vertical ==) firstRow
  pure $ Point {x = startingColumn, y = 0}


loadInput ::
  FilePath
  -> IO Diagram
loadInput path = do
  rawText <- readFile path
  let lines = T.lines rawText
      rows = S.fromList . T.unpack <$> lines
  pure $ S.fromList rows


part1 ::
  FilePath
  -> IO ()
part1 path = do
  diagram <- loadInput path
  let Just start = findStart diagram
      path = S.unfoldr (iteration diagram) (start, Down)
      cells = S.fromList . catMaybes $ toList path
      letters = S.filter onlyLetters cells
  print letters
  where
    -- may skip the final letter ?
    iteration diagram (p, d) = (\st -> (cell diagram p, st)) <$> step diagram p d

    onlyLetters c
      | c == vertical = False
      | c == emptyCell = False
      | c == turn = False
      | c == horizontal = False
      | otherwise = True
part2 ::
  FilePath
  -> IO ()
part2 path = do
  diagram <- loadInput path
  let Just start = findStart diagram
      path = S.unfoldr (iteration diagram) (start, Down)
      cells = catMaybes $ toList path
  print $ length cells
  where
    -- may skip the final letter ?
    iteration diagram (p, d) = (\st -> (cell diagram p, st)) <$> step diagram p d

