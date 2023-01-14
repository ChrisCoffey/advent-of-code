module Utils where

import Protolude

-- Generate all unordered pairs from a list
pairs :: [a] -> [(a,a)]
pairs (a:xs) = ((\b -> (a,b))<$>xs) <> pairs xs
pairs [] = []

notEmpty :: [a] -> Bool
notEmpty = not . null
