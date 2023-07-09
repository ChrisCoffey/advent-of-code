module TwentySeventeen.Day12 where

import Protolude
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Graph (components)

type Graph = M.Map Text [Text]
type Components = M.Map Text Int

connectedComponents ::
  Graph
  -> Components
connectedComponents graph =
  snd $ M.foldlWithKey addToComponent (1, M.empty) graph
  where
    addToComponent :: (Int, Components) -> Text -> [Text] -> (Int, Components)
    addToComponent (componentNumber, components) nodeId edges =
      case M.lookup nodeId components of
        Just component ->
          (componentNumber, components)
        Nothing ->
          let
            newComponent = connectedNodes S.empty graph [nodeId]
            components' = foldl (\acc i -> M.insert i componentNumber acc) components newComponent
          in (componentNumber+1, components')

connectedNodes ::
  S.Set Text
  -> Graph
  -> [Text]
  -> S.Set Text
connectedNodes seen _ [] = seen
connectedNodes seen graph (nodeId:frontier)
  | S.member nodeId seen =
    connectedNodes seen graph frontier
  | otherwise = let
    edges = graph M.! nodeId
    seen' = S.insert nodeId seen
    in connectedNodes seen' graph (frontier<>edges)

parseInput ::
  FilePath
  -> IO Graph
parseInput file = do
  raw <- readFile file
  let xs = lines raw
      parts = T.splitOn " <-> " <$> xs
      nodes = (\[i,rawList] -> (i, (T.splitOn ", " rawList) )) <$> parts
      graph = M.fromList nodes
  pure graph

part1 ::
  FilePath
  -> IO Int
part1 file = do
  graph <- parseInput file
  let components = connectedComponents graph
      component0 = components M.! "0"
      group0 = filter ((== component0) . snd) $ M.assocs components
  pure $ length group0

part2 ::
  FilePath
  -> IO Int
part2 file = do
  graph <- parseInput file
  let components = connectedComponents graph
      groups = groupBy (\ l r -> snd l == snd r) . sortOn snd $ M.assocs components
  pure $ length groups
