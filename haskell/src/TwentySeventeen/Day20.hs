module TwentySeventeen.Day20 where

import Protolude
import Data.Text (splitOn, unpack)
import Text.Read (read)
import Data.Version (parseVersion)
import Data.Sequence (mapWithIndex, fromList)
import TwentySeventeen.Day11 (distance)
import TwentySeventeen.Day14 (input)


data Particle =
  Particle {
    location :: (Int, Int, Int)
  , velocity :: (Int, Int, Int)
  , acceleration :: (Int, Int, Int)
  , particleId :: Int
  }
  deriving (Eq, Ord, Show)

-- Simulate the particles
-- Can this be done more efficently?
stepForward ::
  Int
  -> Particle
  -> Particle
stepForward timeSteps particle =
  particle {
    location = (futurePosition x vx ax, futurePosition y vy ay, futurePosition z vz az)
  , velocity = (vx + (ax * timeSteps), vy + (ay * timeSteps), vz + (az * timeSteps))
  }
  where
    futurePosition coord v a = coord + (v*timeSteps) + ((a* (timeSteps^2)) `div` 2)

    (x, y, z) = location particle
    (vx, vy, vz) = velocity particle
    (ax, ay, az) = acceleration particle

singleStep ::
  Particle
  -> Particle
singleStep particle =
  particle {
    location =  (x + vx', y + vy', z + vz')
    , velocity = velocity'
  }
  where
    (x, y, z) = location particle
    (vx, vy, vz) = velocity particle
    (ax, ay, az) = acceleration particle
    velocity'@(vx', vy', vz') = (vx + ax, vy + ay, vz + az)

-- Find the particle closest to the origin
closestParticle :: Seq Particle -> Particle
closestParticle  = minimumBy (compare `on` distanceFromOrigin)

distanceFromOrigin ::
  Particle
  -> Int
distanceFromOrigin particle =
  (abs x) + (abs y) + (abs z)
  where
    (x, y, z) = location particle

removeCollissions ::
  Seq Particle
  -> Seq Particle
removeCollissions =
  fromList . removeCollisions . findCollisions . toList
  where
    findCollisions = groupBy (\l r -> location l == location r) . sortOn location
    removeCollisions = concat . filter ((1 == ). length)


part1 :: FilePath -> IO (Seq Particle)
part1 path = do
  input <- loadData path
  pure $ simulate 10000000 input
  where
    simulate n particles = stepForward n <$> particles


part2 :: FilePath -> Int -> IO (Seq Particle)
part2 path numSteps =
  simulate numSteps <$> loadData path
  where
    simulate 0 particles = particles
    simulate n particles = simulate (n - 1) . removeCollissions $ singleStep <$> particles

-- data import boilerplate

loadData ::
  FilePath
  -> IO (Seq Particle)
loadData path = do
  rawLines <- lines <$> readFile path
  let segments = splitOn ", "  <$> rawLines
  pure . mapWithIndex parseParticle $ fromList segments
  where
    parseParticle index [p, v, a] =
      Particle {
          location = parseVector p
        , velocity = parseVector v
        , acceleration = parseVector a
        , particleId = index
        }

    parseVector txt = let
      [first, second, third] = splitOn "," txt
      x = read (drop 3 $ unpack first)
      y = read $ unpack second
      z = read $ takeWhile ('>' /=) (unpack third)
      in (x,y,z)


