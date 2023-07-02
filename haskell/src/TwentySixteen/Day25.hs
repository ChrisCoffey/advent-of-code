module TwentySixteen.Day25 where

import Protolude
import Text.Read (read)

import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM

-- a =0, b = 1, c = 2, d = 3
type Registers = IM.IntMap Int

data Instruction
  = Cpy {cpyValue :: Either Int Int, cpyTo :: Int} -- Left = Register, Right = value
  | Inc {incRegister :: Int}
  | Dec {decRegister :: Int}
  | Jnz {registerCheck :: Either Int Int, jumpOffset :: Int}
  | Out {outRegister :: Int}
  deriving (Show)

-- A program is an instrution pointer, the instructions, and the program's memory (registers)
newtype Program = Program (Int, IM.IntMap Instruction, Registers)

run ::
  Program
  -> [Int]
run (Program (instPtr, instructions, registers)) = let
    instruction = instructions IM.! instPtr
    (reg', instPtr', out) = interpret (registers, instPtr) instruction
    in case out of
      Just v ->
        v : run (Program (instPtr', instructions, reg'))
      Nothing ->
        run (Program (instPtr', instructions, reg'))
  where
    (maxInstr, _) = IM.findMax instructions



-- Implementation of each instruction
interpret ::
  (Registers, Int) -- registers & instruction pointer
  -> Instruction
  -> (Registers, Int, Maybe Int)
interpret (registers, instPtr) (Cpy (Left register) to) = let
  value = registers IM.! register
  in (IM.insert to value registers, instPtr +1, Nothing)
interpret (registers, instPtr) (Cpy (Right value) to) =
  (IM.insert to value registers, instPtr +1, Nothing)
interpret (registers, instPtr) (Inc register) = let
  value = registers IM.! register
  in (IM.insert register (value +1) registers, instPtr +1, Nothing)
interpret (registers, instPtr) (Dec register) = let
  value = registers IM.! register
  in (IM.insert register (value - 1) registers, instPtr +1, Nothing)
interpret (registers, instPtr) (Jnz (Left from) offset) = let
  regValue = registers IM.! from
  in if regValue == 0
     then (registers, instPtr +1, Nothing)
     else (registers, instPtr + offset, Nothing)
interpret (registers, instPtr) (Jnz (Right value) offset) =
  if value == 0
  then (registers, instPtr +1, Nothing)
  else (registers, instPtr + offset, Nothing)
interpret (registers, instPtr) (Out register) = let
  value = registers IM.! register
  in (registers, instPtr + 1, Just value)

parseInstructions ::
  FilePath
  -> Int
  -> IO Program
parseInstructions fileName aVal = do
  rawLines <- lines <$> readFile fileName
  let rawWords = words <$> rawLines
      rawInstrutions = toInstruction <$> rawWords
      instructions = IM.fromList $ zip [0..] rawInstrutions
      registers = IM.fromList $ [(0, aVal), (1,0), (2, 0), (3, 0)]
  pure $ Program (0, instructions, registers)
  where
    toInstruction :: [Text] -> Instruction
    toInstruction ["cpy",from, r] =
      if charToRegister from >= 0
      then Cpy (Left $ charToRegister from) (charToRegister r)
      else Cpy (Right (read $ T.unpack from)) (charToRegister r)

    toInstruction ["jnz", from, offset] =
      if charToRegister from >= 0
      then Jnz (Left $ charToRegister from) (read $ T.unpack offset)
      else Jnz (Right (read $ T.unpack from)) (read $ T.unpack offset)

    toInstruction ["inc", reg] =
      Inc (charToRegister reg)

    toInstruction ["dec", reg] =
      Dec (charToRegister reg)

    toInstruction ["out", reg] =
      Out (charToRegister reg)


    charToRegister "a" = 0
    charToRegister "b" = 1
    charToRegister "c" = 2
    charToRegister "d" = 3
    charToRegister _ = -1


go ::
  FilePath
  -> IO [[Int]]
go fileName = do
  progs <- traverse (parseInstructions fileName) [151..200]
  let vals = (take 10 . run) <$> progs
      valid = filter (\xs -> take 5 xs == [0,1,0,1,0]) vals
  pure vals


