module TwentySeventeen.Day15 where

import Protolude


generator factor n = (n * factor) `mod` 2147483647

generatorA ::
  Int
  -> Int
generatorA = generator 16807

generatorB ::
  Int
  -> Int
generatorB = generator 48271

lowest16Bits = 65536

checkPair ::  (Int, Int, Int) -> Int -> (Int, Int, Int)
checkPair (a, b, acc) n =  let
  a' = generatorA a
  b' = generatorB b
  x = if a `mod` lowest16Bits == b `mod` lowest16Bits then 1 else 0
  in (deepseq a' a', deepseq b' b', acc+ deepseq x x)

pairs a b = (generatorA a, generatorB b): pairs (generatorA a) (generatorB b)

matching (a,b) = a `mod` lowest16Bits == b `mod` lowest16Bits

part1 = length . filter matching . take 40000000 $ pairs 512 191

divisibleBy n = filter (\y -> y `mod` n == 0)

pairs2 a b = zip (divisibleBy 8 (genB b)) $ divisibleBy 4 (genA a)
  where
    genA a' = generatorA a': genA (generatorA a')

    genB b' = generatorB b': genB (generatorB b')

part2 = length . filter matching . take 5000000 $ pairs2 512 191


