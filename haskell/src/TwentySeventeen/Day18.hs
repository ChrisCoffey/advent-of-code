module TwentySeventeen.Day18 where

import Protolude
import Prelude (read)
import Data.Char (isLetter)
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T

newtype Registers = Registers (Map Char Int)
  deriving (Eq, Ord, Show)

data Val = Reg Char | Lit Int
  deriving (Eq, Ord, Show)

data Instruction
  = Snd Val
  | Set {register:: Val, value:: Val}
  | Add {register:: Val, value:: Val}
  | Multiply {register:: Val, value:: Val}
  | Mod {register:: Val, value:: Val}
  | Recover {register:: Val}
  | JumpGZero {register:: Val, value:: Val}
  deriving (Eq, Ord, Show)

data Program = Prog {
  registers :: Registers
  , instructionPointer :: Int
  , instructions :: Seq Instruction
  , lastPlayed :: Maybe Int
  , recovered :: [Maybe Int]
  }
  deriving (Eq, Ord, Show)


interpret ::
  Program
  -> Maybe Program
interpret program = do
  instruction <- instructions program S.!? ip
  case instruction of
    Snd v ->
      Just $ program {instructionPointer = ip+1, lastPlayed = Just (valueAt v)}

    Set (Reg r) val ->
      Just $ program {
        instructionPointer = ip+1,
        registers = Registers (M.insert r (valueAt val) reg)
        }

    Add (Reg r) val ->
      Just $ program {
        instructionPointer = ip+1,
        registers = Registers (
          M.insert r (valueAt (Reg r) + valueAt val) reg
          )
        }

    Multiply (Reg r) val ->
      Just $ program {
        instructionPointer = ip+1,
        registers = Registers (
          M.insert r (valueAt (Reg r) * valueAt val) reg
          )
        }

    Mod (Reg r) val ->
      Just $ program {
        instructionPointer = ip+1,
        registers = Registers (
          M.insert r (valueAt (Reg r) `mod` valueAt val) reg
          )
        }

    Recover r ->
      if valueAt r == 0
      then Just $ program { instructionPointer = ip+1 }
      else Just $
        program {
          instructionPointer = ip+1,
          recovered = (lastPlayed program):(recovered program)
        }

    JumpGZero v offset ->
      if valueAt v <= 0
      then Just $ program { instructionPointer = ip+1 }
      else Just $
        program {
          instructionPointer = ip+ valueAt offset
        }

  where
    ip = instructionPointer program
    (Registers reg) = registers program

    valueAt (Lit x) = x
    valueAt (Reg r) = fromMaybe 0 $ M.lookup r reg

parseInput ::
  FilePath
  -> IO [Instruction]
parseInput path = do
  rawLines <- lines <$> readFile path
  let commands =  parseCommand . map T.unpack . T.split (== ' ') <$> rawLines
  pure commands
  where
    parseCommand ["set", a, b] = Set (parseVal a) (parseVal b)
    parseCommand ["add", a, b] = Add (parseVal a) (parseVal b)
    parseCommand ["mul", a, b] = Multiply (parseVal a) (parseVal b)
    parseCommand ["mod", a, b] = Mod (parseVal a) (parseVal b)
    parseCommand ["snd", a] = Snd (parseVal a)
    parseCommand ["rcv", a] = Recover (parseVal a)
    parseCommand ["jgz", a, b] = JumpGZero (parseVal a) (parseVal b)

    parseVal str@(c:rest)
      | isLetter c = Reg c
      | otherwise = Lit $ read str

part1 file = do
  commands <- parseInput file
  let initialProgram = Prog {
    registers = Registers M.empty
  , instructionPointer = 0
  , instructions = S.fromList commands
  , lastPlayed = Nothing
  , recovered = []
  }
      result = until halt (\(Just p) -> interpret p) (Just initialProgram)
  pure result

  where
    halt Nothing = True
    halt (Just prog) = not . null $ recovered prog




