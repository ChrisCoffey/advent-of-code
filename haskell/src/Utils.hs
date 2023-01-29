module Utils where

import Protolude

-- Generate all unordered pairs from a list
pairs :: [a] -> [(a,a)]
pairs (a:xs) = ((\b -> (a,b))<$>xs) <> pairs xs
pairs [] = []

notEmpty :: [a] -> Bool
notEmpty = not . null

choose :: [a] -> Int -> [[a]]
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n = (map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

pairwisePerms :: [a] -> [b] -> [(a, b)]
pairwisePerms [] _ = []
pairwisePerms _ [] = []
pairwisePerms (x: xs) (y: ys) = (x, y) : ((pairwisePerms (x:xs) ys) <> pairwisePerms xs (y: ys))
