module Chips.Utils where
import Chips.Imports

join elem list = concat $ intersperse elem list

for :: [a] -> (a -> b) -> [b]
for = flip map

count :: Eq a => a -> [a] -> Int
count x list = length $ filter (==x) list

withIndices :: [a] -> [(a, Int)]
withIndices arr = zip arr [0..(length arr - 1)]

(//) :: Integral a => a -> a -> a
a // b = floor $ (fromIntegral a) / (fromIntegral b)

($=) ref val = modifyIORef ref (const val)

setValue :: [a] -> Int -> (a -> a) -> [a]
setValue list index func = (take (index) list) ++ [func (list !! index)] ++ (drop (index+1) list)

idx i = to (\x -> x !! i)

whenM func val action = do
  val_ <- use val
  when (func val_) action

with val action = do
  val_ <- use val
  action val_
