module TwentySeventeen.Day16 where

import Protolude hiding (rotate)
import Protolude.Error (error)
import Data.Foldable (length)
import Data.String (String)
import qualified Data.Set as Set
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text (strip)

newtype Index = Index Int
  deriving (Eq, Ord, Show)

newtype ByName = Name Char
  deriving (Eq, Ord, Show)

data Instruction
  = Spin Int
  | Exchange Index Index
  | Partner ByName ByName
  deriving (Eq, Ord, Show)


initialSeq = S.fromList ['a'..'p']

interpret ::
  S.Seq Char
  -> Instruction
  -> S.Seq Char
interpret chars (Spin n) =
  chars `rotate` n
interpret chars (Exchange (Index i) (Index j)) = let
  a = S.index chars i
  b = S.index chars j
  chars' = S.update j a chars
  in S.update i b chars'
interpret chars (Partner (Name a) (Name b)) = let
  Just i = S.elemIndexL a chars
  Just j = S.elemIndexR b chars
  in interpret chars (Exchange (Index i) (Index j))


lookForCycle ::
  S.Seq Instruction
  -> S.Seq Char
  -> Set (S.Seq Char)
  -> Int
lookForCycle inst chars seen
  | chars `Set.member` seen = Set.size seen
  | otherwise = let
    seen' = Set.insert chars seen
    chars' = foldl' interpret chars inst
    in lookForCycle inst chars' seen'

part1 = do
  input <- parseInput "../data/2017/16.txt"
  pure $ foldl' interpret initialSeq input


part2 = do
  input <- parseInput "../data/2017/16.txt"
  let firstCycle = lookForCycle input initialSeq Set.empty
      loops = 1000000000 `mod` firstCycle
      result = foldl' (dance input) initialSeq [1..loops]
  pure result
  where
    dance inst chars _ = foldl' interpret chars inst

parseInput ::
  FilePath
  -> IO (S.Seq Instruction)
parseInput path = do
  raw <- T.strip <$> readFile path
  let elements = T.split (\c -> c == ',') raw
      result = parseElement <$> elements
  pure $ S.fromList result
  where
    parseElement txt =
      case T.unpack txt of
        (['p', a, '/', b]) ->
          Partner (Name a) (Name b)
        ('x':rest) -> let
          i = takeWhile (\c -> c /= '/') rest
          j = tail $ dropWhile (\c -> c /= '/') rest
          in Exchange (Index $ toInt i) (Index $ toInt j)
        ('s':rest) -> let
          n = toInt rest
          in Spin n
        e -> error ("Invalid input element: " <> T.pack e)

toInt ::
  String
  -> Int
toInt str = maybe (error "could not parse") fst . head $ reads str

tail (x:xs) = xs


rotate ::
  S.Seq a
  -> Int
  -> S.Seq a
rotate xs i = let
  len = S.length xs
  back = S.drop (len - i) xs
  front = S.take (len - i) xs
  in back <> front
