module TwentySixteen.Day15 where

import Protolude

type DiscPositions = (Int, Int, Int, Int, Int, Int)
type DiscPositions2 = (Int, Int, Int, Int, Int, Int, Int)

solve :: DiscPositions -> Int
solve (a, b, c, d, e, f)
  | solved = 0
  | otherwise = 1 + solve (a+1, b+1, c+1, d+1, e+1, f+1)
  where
    solved = a `mod` 7 == 7 - 1 &&
             b `mod` 13 == 13 - 2 &&
             c `mod` 3 == 3 - 3 &&
             d `mod` 5 == 5 - 4 &&
             e `mod` 17 == 17 - 5 &&
             f `mod` 19 == 19 - 6

solve2 :: DiscPositions2 -> Int
solve2 (a, b, c, d, e, f, g)
  | solved = 0
  | otherwise = 1 + solve2 (a+1, b+1, c+1, d+1, e+1, f+1, g+1)
  where
    solved = a `mod` 7 == 7 - 1 &&
             b `mod` 13 == 13 - 2 &&
             c `mod` 3 == 3 - 3 &&
             d `mod` 5 == 5 - 4 &&
             e `mod` 17 == 17 - 5 &&
             f `mod` 19 == 19 - 6 &&
             g `mod` 11 == 11 - 7

