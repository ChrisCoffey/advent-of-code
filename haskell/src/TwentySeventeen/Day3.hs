module TwentySeventeen.Day3 where

import Protolude

import qualified Data.Map as M
import TwentySixteen.Day21 (HashOp(a))
import Data.Text (measureOff)

newtype Point = Point (Int, Int)
  deriving (Eq, Ord, Show)

-- Sort of cheated here. I realized the sequence could be looked up in the OIES. Saved me
-- the tedium of writing the spiral algorithm
