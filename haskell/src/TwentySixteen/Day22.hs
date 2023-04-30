module TwentySixteen.Day22 where

import Protolude
import qualified Data.Text as Text
import qualified Data.Sequence as Seq
import Data.List ((!!))

type Coords = (Int, Int)
data Node = Node {
    coords :: Coords
  , size :: Int
  , used :: Int
  , avail :: Int
  }
  deriving (Eq, Ord, Show)

toNode ::
  Text
  -> Node
toNode rawLine = let
  components = stripSpaces rawLine
  in Node {
    coords = coordinates $ components !! 0
  , size = volumeAmount $ components !! 1
  , used = volumeAmount $ components !! 2
  , avail = volumeAmount $ components !! 3
  }

   where
    stripSpaces :: Text.Text -> [Text.Text]
    stripSpaces = filter (not . Text.null) . Text.splitOn " "

    coordinates :: Text.Text -> Coords
    coordinates path = let
     [_, x, y] = Text.splitOn "-" path
     (Just x') = readMaybe $ Text.drop 1 x
     (Just y') = readMaybe $ Text.drop 1 y
     in (x', y')

    volumeAmount amt = let
      raw = Text.dropEnd 1 amt
      (Just amt') = readMaybe raw
      in amt'


allNodes ::
  Text
  -> [Node]
allNodes rawText = let
  lines = filter (not . Text.null) $ Text.splitOn "\n" rawText
  nodeLines = drop 2 lines
  in toNode <$> nodeLines

viablePairs ::
  Node
  -> [Node]
  -> [(Node, Node)]
viablePairs nodeA nodes =
  foldl' checkViable [] nodes
  where
    checkViable acc nodeB =
      go nodeA nodeB $ go nodeB nodeA acc
      where
        go a b acc
          -- Guards
          | coords a == coords b = acc
          | used a <= 0 = acc
          -- valid
          | used a <= avail b = (a, b):acc
          | otherwise = acc




part1 ::
  FilePath
  -> IO Int
part1 path = do
  raw <- readFile path
  let nodes = allNodes raw
      pairs = calcPairs nodes
  print nodes
  pure $ length pairs
  where
    calcPairs [] = []
    calcPairs (a:rest) = (viablePairs a rest) <> calcPairs rest



