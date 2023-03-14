module TwentySixteen.Day21 where

import Protolude

import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P hiding (space)
import Data.Graph (path)

data HashOp =
    Move { source:: Int, dest :: Int }
  | Reverse { start:: Int, end :: Int }
  | SwapLetter { a :: Char, b :: Char}
  | SwapPosition {source :: Int, dest :: Int}
  | RotateRight {n :: Int}
  | RotateLeft {n :: Int}
  | RotatePosition { letter :: Char}
  deriving (Eq, Ord, Show)

type ScrambleString = S.Seq Char

testInput = S.fromList ['a', 'b', 'c', 'd', 'e']
testInput2 = S.fromList ['d', 'e', 'c', 'a', 'b']
input = S.fromList ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
input2 = S.fromList "fbgdceah"

part1 ::
  FilePath
  -> IO ()
part1 path = do
  raw <- readFile path
  case parseInput raw of
    Left bundle -> print (T.pack $ P.errorBundlePretty bundle)
    Right ops -> print $ foldl scramble input ops

part2 ::
  FilePath
  -> IO ()
part2 path = do
  raw <- readFile path
  case parseInput raw of
    Left bundle -> print (T.pack $ P.errorBundlePretty bundle)
    Right ops -> print $ foldl unscramble input2 (reverse ops)


scramble ::
  ScrambleString
  -> HashOp
  -> ScrambleString
scramble txt (Move {source,dest}) = let
  c = S.index txt source
  txt' = S.deleteAt source txt
  in S.insertAt dest c txt'

scramble txt (Reverse {start , end}) = let
  front = S.take start txt
  reversedChunk = S.reverse $ S.take ((end - start) +1) (S.drop start txt)
  back = S.drop (end+1) txt
  in front <> reversedChunk <> back

scramble txt (SwapLetter {a, b}) =
  map (\c -> if c == a then b else if c == b then a else c) txt

scramble txt (SwapPosition {source, dest}) = let
  s = S.index txt source
  d = S.index txt dest
  txt' = S.update source d txt
  in S.update dest s txt'

scramble txt (RotateRight {n}) =
  if n <= 0
  then txt
  else let
    (rest S.:> a) = S.viewr txt
    in scramble (a S.<| rest) (RotateRight {n = n-1})

scramble txt (RotateLeft {n}) =
  if n <= 0
  then txt
  else let
    (a S.:< rest) = S.viewl txt
    in scramble (rest S.|> a) (RotateLeft {n = n-1})

scramble txt (RotatePosition {letter}) = let
  (Just i) = S.elemIndexL letter txt
  n = if i >= 4 then i+2 else i+1
  in scramble txt (RotateRight {n})


unscramble ::
  ScrambleString
  -> HashOp
  -> ScrambleString
unscramble txt (Move {source, dest}) =
  scramble txt (Move {source= dest, dest = source})

unscramble txt (RotateRight {n}) =
  scramble txt (RotateLeft {n})

unscramble txt (RotateLeft {n}) =
  scramble txt (RotateRight {n})

unscramble txt (RotatePosition {letter}) = let
  (Just i) = S.elemIndexL letter txt
  n = (i `div` 2) + (if odd i || i == 0 then 1 else 5)
  in scramble txt (RotateLeft {n})

unscramble txt op =
  scramble txt op


-- Parsing below
type Parser = P.Parsec Void Text
type ParserError = P.ParseErrorBundle Text Void
parseInput ::
  Text
  -> Either ParserError [HashOp]
parseInput input = P.parse (operationParser `P.endBy` P.eol) "" input

operationParser :: Parser HashOp
operationParser =
  moveParser <|>
  reverseParser <|>
  swapLParser <|>
  swapPosParser <|>
  rotateRParser <|>
  rotatePosParser <|>
  rotateLParser

moveParser :: Parser HashOp
moveParser = do
  _ <- P.string "move position"
  _ <- P.space
  src <- P.decimal
  _ <- P.space
  _ <- P.string "to position"
  _ <- P.space
  dest <- P.decimal
  pure $ Move {source = src, dest = dest}

reverseParser :: Parser HashOp
reverseParser = do
  _ <- P.string "reverse positions"
  _ <- P.space
  src <- P.decimal
  _ <- P.space
  _ <- P.string "through"
  _ <- P.space
  dest <- P.decimal
  pure $ Reverse {start = src, end = dest}

swapLParser :: Parser HashOp
swapLParser = do
  _ <- P.string "swap letter"
  _ <- P.space
  a <- P.alphaNumChar
  _ <- P.space
  _ <- P.string "with letter"
  _ <- P.space
  b <- P.alphaNumChar
  pure $ SwapLetter {a= a, b = b}

swapPosParser :: Parser HashOp
swapPosParser = do
  _ <- P.string "swap position"
  _ <- P.space
  src <- P.decimal
  _ <- P.space
  _ <- P.string "with position"
  _ <- P.space
  dest <- P.decimal
  pure $ SwapPosition {source = src, dest = dest}

rotateRParser :: Parser HashOp
rotateRParser = do
  _ <- P.string "rotate right"
  _ <- P.space
  n <- P.decimal
  _ <- P.space
  _ <- P.string "step"
  _ <- P.optional (P.string "s")
  pure $ RotateRight {n}

rotateLParser :: Parser HashOp
rotateLParser = do
  _ <- P.string "rotate left"
  _ <- P.space
  n <- P.decimal
  _ <- P.space
  _ <- P.string "step"
  _ <- P.optional (P.string "s")
  pure $ RotateLeft {n}


rotatePosParser :: Parser HashOp
rotatePosParser = do
  _ <- P.string "rotate based on position of letter"
  _ <- P.space
  letter <- P.alphaNumChar
  pure $ RotatePosition {letter}
