module TwentySixteen.Day23 where

import Protolude
import Text.Read (read)

import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM

-- a =0, b = 1, c = 2, d = 3
type Registers = IM.IntMap Int

-- Left = Register, Right = value
data Instruction
  = Cpy {cpyValue :: Either Int Int, cpyTo :: Either Int Int}
  | Inc {incRegister :: Int}
  | Dec {decRegister :: Int}
  | Jnz {registerCheck :: Either Int Int, jumpOffset :: Either Int Int}
  | Tgl {targetRegister :: Int}
  deriving (Show)

-- A program is an instrution pointer, the instructions, and the program's memory (registers)
newtype Program = Program (Int, IM.IntMap Instruction, Registers)

run ::
  Program
  -> Int
  -> Int
run prog@(Program (instPtr, instructions, registers)) resultRegister
  | instPtr > maxInstr = registers IM.! resultRegister
  | otherwise = let
    instruction = instructions IM.! instPtr
    -- prog' = trace (((show instruction) :: T.Text) <> (show registers)) $  interpret prog instruction
    prog' = interpret prog instruction
    in run prog' resultRegister
  where
    (maxInstr, _) = IM.findMax instructions



-- Implementation of each instruction
interpret ::
  Program -- registers & instruction pointer
  -> Instruction
  -> Program
interpret (Program (instPtr, instructions, registers)) (Cpy (Left register) (Left to)) = let
  value = registers IM.! register
  in Program (instPtr + 1, instructions, IM.insert to value registers)
interpret (Program (instPtr, instructions, registers)) (Cpy (Right value) (Left to)) =
  Program (instPtr + 1, instructions, IM.insert to value registers)
interpret (Program (instPtr, instructions, registers)) (Inc register) = let
  value = registers IM.! register
  in Program (instPtr +1, instructions, IM.insert register (value +1) registers)
interpret (Program (instPtr, instructions, registers)) (Dec register) = let
  value = registers IM.! register
  in Program (instPtr + 1, instructions, IM.insert register (value - 1) registers)
interpret (Program (instPtr, instructions, registers)) (Jnz (Left from) offset) = let
  regValue = registers IM.! from
  jumpDistance = case offset of
                    Left r -> registers IM.! r
                    Right v -> v
  in if regValue == 0
     then Program (instPtr + 1, instructions, registers)
     else Program (instPtr + jumpDistance, instructions, registers)
interpret (Program (instPtr, instructions, registers)) (Jnz (Right value) offset) = let
  jumpDistance = case offset of
                    Left r -> registers IM.! r
                    Right v -> v
  in if value == 0
     then Program (instPtr + 1, instructions, registers)
     else Program (instPtr + jumpDistance, instructions, registers)
interpret (Program (instPtr, instructions, registers)) (Tgl reg) = let
  regValue = registers IM.! reg
  targetInst = instructions IM.! (instPtr + regValue)
  (maxInstr, _) = IM.findMax instructions
  newInst = case targetInst of
              Inc r -> Dec r
              Dec r -> Inc r
              Tgl r -> Inc r
              Cpy r n -> Jnz r n
              Jnz r n -> Cpy r n
  instructions' = IM.insert (instPtr + regValue) newInst instructions
  in if (instPtr + regValue) > maxInstr
     then Program (instPtr + 1, instructions, registers)
     else Program (instPtr +1, instructions', registers)
interpret (Program (instPtr, instructions, registers)) _ =
  Program (instPtr + 1, instructions, registers)


parseInstructions ::
  FilePath
  -> IO Program
parseInstructions fileName = do
  rawLines <- lines <$> readFile fileName
  let rawWords = words <$> rawLines
      rawInstrutions = toInstruction <$> rawWords
      instructions = IM.fromList $ zip [0..] rawInstrutions
      registers = IM.fromList $ [(0, 7), (1,0), (2, 1), (3, 0)]
  pure $ Program (0, instructions, registers)
  where
    toInstruction :: [Text] -> Instruction
    toInstruction ["cpy",from, r] =
      if charToRegister from >= 0
      then Cpy (Left $ charToRegister from) (Left $ charToRegister r)
      else Cpy (Right (read $ T.unpack from)) (Left $ charToRegister r)

    toInstruction ["jnz", from, offset] = let
      source = if charToRegister from >= 0
               then Left $ charToRegister from
               else Right (read $ T.unpack from)
      offset' = if charToRegister offset >= 0
               then Left $ charToRegister offset
               else Right (read $ T.unpack offset)
      in Jnz source offset'

    toInstruction ["inc", reg] =
      Inc (charToRegister reg)

    toInstruction ["dec", reg] =
      Dec (charToRegister reg)

    toInstruction ["tgl", reg] =
      Tgl (charToRegister reg)


    charToRegister "a" = 0
    charToRegister "b" = 1
    charToRegister "c" = 2
    charToRegister "d" = 3
    charToRegister _ = -1

