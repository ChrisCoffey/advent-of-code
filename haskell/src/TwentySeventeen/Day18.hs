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
  = Send Val
  | Set {register:: Val, value:: Val}
  | Add {register:: Val, value:: Val}
  | Multiply {register:: Val, value:: Val}
  | Mod {register:: Val, value:: Val}
  | Receive {register:: Val}
  | JumpGZero {register:: Val, value:: Val}
  deriving (Eq, Ord, Show)

type Instructions = Seq Instruction
type MessageQueue = Seq Int

data Program = Prog {
    registers :: Registers
  , instructionPointer :: Int
  , sentCount :: Int
  }
  deriving (Eq, Ord, Show)

data ProgramPair = ProgramPair {
    focused :: Program
  , sleeping :: Program
  , instructions :: Instructions
  , messageQueues :: (MessageQueue, MessageQueue)
  } deriving (Eq, Ord, Show)


interpret ::
  ProgramPair
  -> Maybe ProgramPair
interpret pp@(ProgramPair focused sleeping instructions (ownQueue, sleepingQueue)) = do
  instruction <- instructions S.!? ip
  case instruction of
    Send v -> let
      value = valueAt v
      sleepingQueue' = sleepingQueue S.|> value
      focused' = incrementSentCount $ incrementInstructionPointer focused
      in Just $ pp {
        focused = focused' ,
        messageQueues = (ownQueue, sleepingQueue')
      }

    Set (Reg r) val ->
      let
      registers' = M.insert r (valueAt val) reg
      focused' = incrementInstructionPointer $ updateRegisters focused registers'
      in Just $ pp { focused = focused' }

    Add (Reg r) val ->
      let
      registers' = M.insert r (valueAt (Reg r) + valueAt val) reg
      focused' = incrementInstructionPointer $ updateRegisters focused registers'
      in Just $ pp { focused = focused' }

    Multiply (Reg r) val ->
      let
      registers' = M.insert r (valueAt (Reg r) * valueAt val) reg
      focused' = incrementInstructionPointer $ updateRegisters focused registers'
      in Just $ pp { focused = focused' }

    Mod (Reg r) val ->
      let
      registers' = M.insert r (valueAt (Reg r) `mod` valueAt val) reg
      focused' = incrementInstructionPointer $ updateRegisters focused registers'
      in Just $ pp { focused = focused' }


    -- When blocked to receive a message, context switch to the other program
    Receive _ | S.null ownQueue ->
      Just $ pp {
          focused = sleeping
        , sleeping = focused
        , messageQueues = (sleepingQueue, ownQueue)
        }
    Receive (Reg r) ->
      case S.viewl ownQueue of
        S.EmptyL -> Nothing -- error? Deadlock?
        (v S.:< restOfOwnQueue) -> let
          registers' = M.insert r v reg
          focused' = incrementInstructionPointer $ updateRegisters focused registers'
          in Just $ pp { focused = focused', messageQueues = (restOfOwnQueue, sleepingQueue) }


    JumpGZero v offset ->
      let
      newPointer =
        if valueAt v <= 0
        then ip+1
        else ip+ valueAt offset
      focused' = focused { instructionPointer = newPointer }
      in Just $ pp {focused = focused' }

  where
    ip = instructionPointer focused
    (Registers reg) = registers focused

    valueAt (Lit x) = x
    valueAt (Reg r) = fromMaybe 0 $ M.lookup r reg

    incrementInstructionPointer p = p { instructionPointer = ip + 1}
    incrementSentCount p = p { sentCount = sentCount p + 1}
    updateRegisters p newRegisters = p {registers = Registers newRegisters }



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
    parseCommand ["snd", a] = Send (parseVal a)
    parseCommand ["rcv", a] = Receive (parseVal a)
    parseCommand ["jgz", a, b] = JumpGZero (parseVal a) (parseVal b)

    parseVal str@(c:rest)
      | isLetter c = Reg c
      | otherwise = Lit $ read str



--part1 file = do
--  commands <- parseInput file
--  let initialProgram = Prog {
--   registers = Registers M.empty
--  , instructionPointer = 0
--  , instructions = S.fromList commands
--  , lastPlayed = Nothing
--  , recovered = []
--  }
--      result = until halt (\(Just p) -> interpret p) (Just initialProgram)
--  pure result
--  where
--    halt Nothing = True
--    halt (Just prog) = not . null $ recovered prog

part2 file = do
  instructions <- parseInput file
  let
    p0 = Prog {
      registers = Registers $ M.fromList [('p', 0)]
    , instructionPointer = 0
    , sentCount = 0
    }
    p1 = Prog {
      registers = Registers $ M.fromList [('p', 1)]
    , instructionPointer = 0
    , sentCount = 0
    }
    pp = ProgramPair {
      focused = p0
    , sleeping = p1
    , instructions = S.fromList instructions
    , messageQueues = (S.empty, S.empty)
    }
    result = until halt (\(Just step) -> interpret step) (Just pp)
  pure result

  where
    halt Nothing = True
    halt (Just pp) = inDeadlock pp

    inDeadlock (ProgramPair focused sleeping inst (ownQueue, sleepingQueue))=
      waitingToReceive inst focused &&
      waitingToReceive inst sleeping &&
      S.null sleepingQueue &&
      S.null ownQueue
    -- detect a deadlock by checking target message queue and instruction.
    -- If target queue is empty, program is waiting to receive, and current program
    -- is _also_ waiting to receive, it's a deadlock

    waitingToReceive inst p =
      case inst S.!? instructionPointer p of
        Just (Receive _) -> True
        _ -> False
