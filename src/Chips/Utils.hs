module Chips.Utils where
import Chips.Imports
import Chips.Globals
import Chips.Types

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

with val action = execState action val

eachTile :: ((Tile, Int) -> GameMonad ()) -> GameMonad ()
eachTile action = do
  gs <- get
  forM_ (withIndices (gs ^. tiles)) action
    

chipsLeft gs = length $ filter isChip (_tiles gs)
  where isChip (Chip _) = True
        isChip _        = False

oof :: GameMonad ()
oof = liftIO $ playSound (soundDir ++ "oof.wav") False

guardGodMode action = do
  gm <- use godMode
  when (not gm) action

win :: GameMonad ()
win = liftIO $ playSound (soundDir ++ "win.wav") False

bummer :: GameMonad ()
bummer = liftIO $ playSound (soundDir ++ "bummer.wav") False

onTick :: GameMonad () -> GameMonad ()
onTick action = whenM fst tick action

-- wait 2 ticks before doing this
onDoubleTick :: GameMonad () -> GameMonad ()
onDoubleTick action = whenM (even . snd) tick action

(<||>) :: GameMonad Bool -> GameMonad Bool -> GameMonad Bool
a <||> b = do
  res <- a
  if res
    then return True
    else b

oneOf :: [GameMonad Bool] -> GameMonad Bool
oneOf [] = return False
oneOf (x:[]) = x
oneOf (x:xs) = x <||> (oneOf xs)

once :: GameMonad () -> GameMonad ()
once action = do
    cur  <- liftIO . readIORef $ curLocation
    prev <- liftIO . readIORef $ prevLocation
    when (cur /= prev) action
