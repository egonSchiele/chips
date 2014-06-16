module Chips.Move where
import Chips.Types
import Chips.Utils
import Chips.Imports
import Chips.Globals
import Chips.Position
import Chips.CurrentTile

-- move a tile from one location to another. Generally
-- used to move tanks, frogs, other enemies. If the enemy
-- had a special tile under them (like a button, for example),
-- that special tile won't be erased.
-- When the item gets moved to a location with a button,
-- that button will get pressed. If there's a trap there,
-- the item will now be in the trap.
moveTile :: Int -> Int -> Maybe Direction -> GameMonad Bool
moveTile from to newDir = do
  fromTile <- use $ tileAt (Ix from)
  toTile   <- use $ tileAt (Ix to)
  let newToTile = case toTile of
                    Trap _ _ -> tileUnder .~ fromTile $ toTile
                    _ -> with fromTile $ do
                           case newDir of
                             Just dir_ -> do
                               dir .= dir_
                               tileUnder .= toTile
                             Nothing   -> tileUnder .= toTile
  tileAt (Ix from) .= (fromTile ^. tileUnder)
  tileAt (Ix to) .= newToTile
  when (isButton toTile) $ checkCurTile toTile
  return True

-- given a tile and a direction, move it in that direction
-- if there are no walls or anything. Used to move enemies.
-- The last param is a function that takes a tile type and
-- returns the appropriate action. This is because some
-- enemies respond differently to different tiles.
maybeMoveTile :: Int -> Direction -> Maybe ((Tile, Int) -> GameMonad Bool) -> GameMonad Bool
maybeMoveTile i dir func = do
  let moveIfEmpty moveI = do
        moveTo <- use $ tileAt (Ix moveI)
        case moveTo of
          Empty _           -> moveTile i moveI (Just dir)
          ButtonRed _       -> moveTile i moveI (Just dir)
          ButtonBrown _ _   -> moveTile i moveI (Just dir)
          ButtonBlue _      -> moveTile i moveI (Just dir)
          ButtonGreen _     -> moveTile i moveI (Just dir)
          ToggleDoor True _ -> moveTile i moveI (Just dir)
          Trap _ _          -> moveTile i moveI (Just dir)
          -- flexibility
          x -> case func of
                 Nothing -> return False
                 Just f -> f (x, moveI)
  case dir of
    DirLeft  -> moveIfEmpty (i - 1)
    DirRight -> moveIfEmpty (i + 1)
    DirUp    -> moveIfEmpty (i - boardW)
    DirDown  -> moveIfEmpty (i + boardW)
