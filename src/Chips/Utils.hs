module Chips.Utils where
import Chips.Common

join elem list = concat $ intersperse elem list

for :: [a] -> (a -> b) -> [b]
for = flip map

count :: Eq a => a -> [a] -> Int
count x list = length $ filter (==x) list

indices :: [a] -> [Int]
indices arr = [0..(length arr - 1)]

(//) :: Integral a => a -> a -> a
a // b = floor $ (fromIntegral a) / (fromIntegral b)
