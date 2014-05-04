{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import Chips
import Control.Lens
import Graphics.Gloss hiding (display)
import Chips.Utils

tileSize = 32

tileMap = 
    [[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
     [1, 1, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1],
     [1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1],
     [1, 1, 1, 2, 1, 3, 1, 2, 13, 2, 1, 3, 1, 2, 1, 1, 1],
     [1, 2, 2, 2, 2, 2, 10, 2, 12, 2, 10, 2, 2, 2, 2, 2, 1],
     [1, 2, 1, 4, 1, 11, 1, 1, 1, 1, 1, 9, 1, 4, 1, 2, 1],
     [1, 2, 1, 3, 1, 2, 7, 1, 14, 1, 5, 2, 1, 3, 1, 2, 1],
     [1, 2, 2, 2, 2, 2, 3, 1, 1, 1, 3, 2, 2, 2, 2, 2, 1],
     [1, 2, 1, 3, 1, 2, 7, 1, 1, 1, 5, 2, 1, 3, 1, 2, 1],
     [1, 2, 1, 1, 1, 9, 1, 1, 3, 1, 1, 11, 1, 1, 1, 2, 1],
     [1, 2, 2, 2, 2, 2, 2, 8, 2, 8, 2, 2, 2, 2, 2, 2, 1],
     [1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1],
     [1, 1, 1, 1, 1, 2, 1, 3, 2, 3, 1, 2, 1, 1, 1, 1, 1],
     [1, 1, 1, 1, 1, 2, 1, 1, 2, 6, 1, 2, 1, 1, 1, 1, 1],
     [1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1],
     [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]

boardW = length . head $ tileMap
boardH = length tileMap

playerCoords :: GameState -> (Int, Int)
playerCoords gs = ((floor (p ^. x)) // tileSize, (((boardH * tileSize) - (floor (p ^. y))) // tileSize)-1)
    where p = _player gs
          ts = _tiles gs

currentIdx :: GameState -> Int
currentIdx gs = y_ * boardW + x_
    where (x_,y_) = playerCoords gs

currentTile gs = (_tiles gs) !! (currentIdx gs)
leftTile gs    = (_tiles gs) !! (currentIdx gs - 1)
rightTile gs   = (_tiles gs) !! (currentIdx gs + 1)
upTile gs      = (_tiles gs) !! (currentIdx gs - boardW)
downTile gs    = (_tiles gs) !! (currentIdx gs + boardW)

renderedTiles = renderTileMap tileMap f (tileSize, tileSize)
    where f 1  = Empty def
          f 2  = Wall def
          f 3  = Chip def
          f 4  = KeyYellow def
          f 5  = KeyRed def
          f 6  = KeyGreen def
          f 7  = KeyBlue def
          f 8  = LockYellow def
          f 9  = LockRed def
          f 10 = LockGreen def
          f 11 = LockBlue def
          f 12 = Gate def
          f 13 = GateFinal def
          f 14 = Help def

gameState = GameState renderedTiles (x .~ (8*tileSize) $ y .~ (8*tileSize) $ player_) 1 "LESSON 1" "BDHP" 0 0 0 False def
        where player_ = (Player DirDown def)

main = run "chips challenge" (9 * tileSize, 9 * tileSize) (x -~ (4*tileSize) $ y -~ (4*tileSize) $ gameState) on stepGame

chipsLeft gs = length $ filter isChip (_tiles gs)
  where isChip (Chip _) = True
        isChip _        = False

maybeMove :: (GameState -> Tile) -> GameState -> GameState -> GameState
maybeMove func gs newGs =
    case func gs of
      Wall _ -> gs
      LockRed _    -> if _redKeyCount gs > 0 then newGs else gs
      LockBlue _   -> if _blueKeyCount gs > 0 then newGs else gs
      LockGreen _  -> if _hasGreenKey gs then newGs else gs
      LockYellow _ -> if _yellowKeyCount gs > 0 then newGs else gs
      Gate _       -> if chipsLeft gs == 0 then newGs else gs
      _ -> newGs

on (EventKey (SpecialKey KeyLeft) Down _ _) gs =
    return $ maybeMove leftTile gs $ 
      player.direction .~ DirLeft
      $ player.x -~ tileSize
      $ x +~ tileSize
      $ gs

on (EventKey (SpecialKey KeyRight) Down _ _) gs =
    return $ maybeMove rightTile gs $
      player.direction .~ DirRight
      $ player.x +~ tileSize
      $ x -~ tileSize
      $ gs

on (EventKey (SpecialKey KeyUp) Down _ _) gs =
    return $ maybeMove upTile gs $
      player.direction .~ DirUp
      $ player.y +~ tileSize
      $ y -~ tileSize
      $ gs

on (EventKey (SpecialKey KeyDown) Down _ _) gs =
    return $ maybeMove downTile gs $
      player.direction .~ DirDown
      $ player.y -~ tileSize
      $ y +~ tileSize
      $ gs

on (EventKey (SpecialKey KeySpace) Down _ _) gs =
    return gameState
on _ gs =
    return $ player.direction .~ DirDown $ gs

stepGame _ gs@(LevelComplete _ _) = return gs
stepGame _ gs = do
    let playerIx = currentIdx gs
    let attrs_ = ((gs ^. tiles) !! playerIx) ^. attrs
    let resetTile i = tiles.(ix i) .~ (Empty attrs_) $ gs
    case currentTile gs of
      Chip _ -> return $ resetTile playerIx
      Gate _ -> return $ resetTile playerIx
      KeyYellow _ -> return $ yellowKeyCount +~ 1 $ resetTile playerIx
      KeyBlue _ -> return $ blueKeyCount +~ 1 $ resetTile playerIx
      KeyGreen _ -> return $ hasGreenKey .~ True $ resetTile playerIx
      KeyRed _ -> return $ redKeyCount +~ 1 $ resetTile playerIx
      LockYellow _ -> return $ yellowKeyCount -~ 1 $ resetTile playerIx
      LockBlue _ -> return $ blueKeyCount -~ 1 $ resetTile playerIx
      LockGreen _ -> return $ resetTile playerIx
      LockRed _ -> return $ redKeyCount -~ 1 $ resetTile playerIx
      GateFinal _ -> do
        let lvl = _level gs
        return $ LevelComplete lvl def
      _ -> return gs
