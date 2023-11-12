module TwentySeventeen.Day20 where

import Protolude
import Data.Text (splitOn, unpack)
import Text.Read (read)
import Data.Version (parseVersion)
import Data.Sequence (mapWithIndex, fromList)
import TwentySeventeen.Day11 (distance)


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
    location = (x + vx, y + vy, z + vy)
  , velocity = (vx + ax, vy + ay, vz + az)
  }
  where
    (x, y, z) = location particle
    (vx, vy, vz) = velocity particle
    (ax, ay, az) = acceleration particle

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

-- Filter out those particles that cannot get closer to the origin than they are now
-- stillInContention ::



part1 :: FilePath -> IO (Seq Particle)
part1 path = do
  input <- loadData path
  pure $ simulate 100000 input
  where
    simulate 0 particles = particles
    simulate n particles = simulate (n-1) $ stepForward <$> particles

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


