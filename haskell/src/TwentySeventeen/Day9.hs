module TwentySeventeen.Day9 where

import Protolude
import Data.String (String)
import Data.Text (unpack)
import TwentySixteen.Day21 (HashOp(a))

-- Coroutines that transfer control back and forth while processing
-- the input stream

processGroup ::
  Int
  -> String
  -> Int
processGroup score [] = 0
processGroup score ('{':rest) = score + processGroup (score + 1) rest
processGroup score ('}':rest) = processGroup (score - 1) rest
processGroup score ('<':rest) = processGarbage score rest False
processGroup score (_:rest) = processGroup score rest


processGarbage ::
  Int
  -> String
  -> Bool
  -> Int
processGarbage score (_:rest) True = processGarbage score rest False
processGarbage score ('!':rest) False = processGarbage score rest True
processGarbage score ('>':rest) False = processGroup score rest
processGarbage score (_:rest) False = processGarbage score rest False


run ::
  FilePath
  -> IO Int
run fileName =
  processGroup 1 . unpack <$> readFile fileName


----- Part 2

data InGarbage = In | Out

countGarbage ::
  String
  -> InGarbage
  -> Bool
  -> Int
countGarbage [] _ _ = 0
countGarbage (_:rest) In True = countGarbage rest In False
countGarbage ('<':rest) Out False = countGarbage rest In False
countGarbage ('!':rest) In False = countGarbage rest In True
countGarbage ('>':rest) In False = countGarbage rest Out False
countGarbage (_:rest) In False = 1 + countGarbage rest In False
countGarbage (_:rest) Out False = countGarbage rest Out False




run2 ::
  FilePath
  -> IO Int
run2 fileName =
  (\s -> countGarbage s Out False) . unpack <$> readFile fileName
