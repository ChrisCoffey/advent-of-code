module TwentySixteen.Day17 where

import Protolude

import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (MD5(..))
import Data.Char (digitToInt)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.IntPSQ as Q

testInput :: Text
testInput = "hijkl"

testInput2 :: Text
testInput2 = "ihgpwlah"

testInput3 :: Text
testInput3 = "kglvqrro"

testInput4 :: Text
testInput4 = "ulqzkmiv"

input :: Text
input = "rrrbmfta"


-- (x, y)
newtype Position = Pos (Int, Int)
  deriving (Eq, Ord, Show)

newtype Path = State (Position, Text)
  deriving (Eq, Ord, Show)

type ManhattanDistance = Int

start = Pos (1,4)
end = Pos (4,1) -- Exit right

-- Use A* with heiristic that shorter euclidean distance to end + shorter strings are better
-- Store the frontier in a prioirty queue
-- At each step, pop the most promising (Text, Pos) pair and evaluate it
--  -- Evaluating means checking if it's a winner
--                      computing the MD5
--                      Finding the possible moves _from_ the current position, and enqueeing them

run ::
  Text
  -> Text
run input = let
  (State (_, finalPath)) = search Q.empty (State (start, input))
  in finalPath

search ::
  Q.IntPSQ ManhattanDistance Path
  -> Path
  -> Path
search queue path@(State (pos, txt))
  | atFinish path = path
  | otherwise = let
    queue' = foldr (\p q -> Q.insert (pathKey p) (priority p) p q) queue $ nextSteps path
    Just (_,_, path', nextQueue) = Q.minView queue'
    in search nextQueue path'

runPart2 ::
  Text
  -> Int
runPart2 input =
  maximum $ pathLength <$> longestPath Q.empty (Just (State (start, input)))
  where
    pathLength (State (_, p)) = T.length $ T.drop (T.length input) p

-- accumulate a list of paths
longestPath ::
  Q.IntPSQ ManhattanDistance Path
  -> Maybe Path
  -> [Path]
longestPath queue path =
  case Q.minView queue' of
    Just (_,_, path', nextQueue) | atFinish path' ->
      -- this isn't quite right. This is already a completed path. Need to find the next one?
      path' : longestPath nextQueue Nothing
    Just (_,_, path', nextQueue) ->
      longestPath nextQueue (Just path')
    Nothing ->
      [] -- The queue is empty. We're done

  where
    queue' =
      case path of
        Just pth@(State (pos, txt)) ->
          foldr (\p q -> Q.insert (pathKey p) ( priority p * (-1)) p q) queue $ nextSteps pth
        Nothing ->
          queue

-- What should the key be?
pathKey :: Path -> Int
pathKey (State (_, txt)) = base10MD5 txt

priority p@(State (Pos (x, y), str)) = T.length str * 10000 + manhattanDistance p

manhattanDistance :: Path -> ManhattanDistance
manhattanDistance (State (Pos (x, y), _)) = (abs $ x - 4) + (abs $ y - 1)

atFinish ::
  Path
  -> Bool
atFinish (State (pos, _)) = pos == end

-- Returns the list of VALID moves from the current state
nextSteps ::
  Path
  -> [Path]
nextSteps (State (Pos (x, y), str)) =
  up <> down <> left <> right
  where
    hashedInput = T.take 4 $ md5 str

    closedDoor i =  (<= 10) . digitToInt $ T.index hashedInput i

    up = if y >= 4 || closedDoor 0 then [] else [State (Pos (x, y+1), str<>"U")]
    down = if y <= 1 || closedDoor 1 then [] else [State (Pos (x, y-1), str<>"D")]
    left = if  x <= 1 || closedDoor 2 then [] else [State (Pos (x-1, y), str<>"L")]
    right = if x >= 4 || closedDoor 3 then [] else [State (Pos (x+1, y), str<>"R")]

md5 :: Text -> Text
md5 = T.pack . Protolude.show . hashWith MD5 . T.encodeUtf8

base10MD5 = T.foldl f 0 . md5
  where
    f acc c = digitToInt c + (acc * 100)
