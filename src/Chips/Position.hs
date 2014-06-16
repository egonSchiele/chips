module Chips.Position where
import Chips.Types
import Chips.Utils
import Chips.Imports
import Chips.Globals

playerCoords :: GameState -> (Int, Int)
playerCoords gs = ((floor (p ^. x)) // tileSize, (((boardH * tileSize) - (floor (p ^. y))) // tileSize)-1)
    where p = gs ^. player
          ts = gs ^. tiles

currentIdx :: GameState -> Int
currentIdx gs = y_ * boardW + x_
    where (x_,y_) = playerCoords gs

-- given a tile position, gives you the index
-- of that tile in the tiles array.
tilePosToIndex :: TilePos -> GameMonad Int
tilePosToIndex pos = do
    gs <- get
    return $ posToIndex pos gs

posToIndex :: TilePos -> GameState -> Int
posToIndex pos gs = 
  case pos of
    Current   -> playerIdx
    TileLeft  -> playerIdx - 1
    TileRight -> playerIdx + 1
    TileAbove -> playerIdx - boardW
    TileBelow -> playerIdx + boardW
    Ix i -> i
    Coords (x, y) -> (x - 1) + boardW * (y - 1)
  where playerIdx = currentIdx gs

-- tile :: Functor f => TilePos -> (Tile -> f Tile) -> Tile -> f Tile
tileAt pos = lens (getTile pos) (setTile pos)

getTile :: TilePos -> GameState -> Tile
getTile pos gs = gs ^. tiles.(idx i)
  where i = posToIndex pos gs
        
setTile :: TilePos -> GameState -> Tile -> GameState
setTile pos gs tile = tiles.(ix i).attrs .~ attrs_ $
                      tiles.(ix i) .~ tile $ gs
  where i = posToIndex pos gs
        attrs_ = gs ^. tiles.(idx i).attrs
