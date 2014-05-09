{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import Chips
import Control.Lens
import Graphics.Gloss hiding (display)
import Chips.Utils
import Data.List
import Data.Maybe
tileSize = 32
soundDir = "sounds/"

oof :: IO ()
oof = playSound (soundDir ++ "oof.wav") False

tileMap = 
    [[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 3, 1, 2, 13, 2, 1, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 10, 2, 12, 2, 10, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 4, 1, 11, 1, 1, 1, 1, 1, 9, 1, 4, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 3, 1, 2, 7, 1, 14, 1, 5, 2, 1, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 1, 0, 1, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 3, 1, 2, 7, 1, 1, 1, 5, 2, 1, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 9, 1, 1, 3, 1, 1, 11, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 8, 2, 8, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 3, 2, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 6, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    ,[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]

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
    where f 0  = Empty def -- 0 == where chip will be
          f 1  = Empty def
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

-- | (x, y) of chip's start position (marked as a 0 on the tile map)
chipStart :: (Float, Float)
chipStart = case findIndex (\xs -> 0 `elem` xs) tileMap of
              Nothing -> error "You need to mark where chip will stand in the tilemap. Mark it with a zero (0)."
              Just y -> (fromIntegral . fromJust $ findIndex (==0) (tileMap !! y), fromIntegral $ y + 3)

gameState = GameState renderedTiles (x .~ ((fst chipStart)*tileSize) $ y .~ ((snd chipStart)*tileSize) $ player_) 1 "LESSON 1" "BDHP" 0 0 0 False def
        where player_ = (Player Standing def)

main = do
    print chipStart
    playSound (soundDir ++ "chips01.wav") True
    run "chips challenge" (9 * tileSize, 9 * tileSize) (x .~ ((4 - fst chipStart)*tileSize) $ y .~ ((4 - snd chipStart)*tileSize) $ gameState) on stepGame

chipsLeft gs = length $ filter isChip (_tiles gs)
  where isChip (Chip _) = True
        isChip _        = False

maybeMove :: (GameState -> Tile) -> GameState -> GameState -> IO GameState
maybeMove func gs newGs =
    case func gs of
      Wall _ -> do
        oof
        return gs
      LockRed _    -> if _redKeyCount gs > 0
                        then return newGs
                        else oof >> return gs
      LockBlue _   -> if _blueKeyCount gs > 0
                        then return newGs
                        else oof >> return gs
      LockGreen _  -> if _hasGreenKey gs
                        then return newGs
                        else oof >> return gs
      LockYellow _ -> if _yellowKeyCount gs > 0
                        then return newGs
                        else oof >> return gs
      Gate _       -> if chipsLeft gs == 0
                        then return newGs
                        else oof >> return gs
      _ -> return newGs

on (EventKey (SpecialKey KeyLeft) Down _ _) gs = return $ player.direction .~ DirLeft $ gs

on (EventKey (SpecialKey KeyRight) Down _ _) gs = return $ player.direction .~ DirRight $ gs

on (EventKey (SpecialKey KeyUp) Down _ _) gs = return $ player.direction .~ DirUp $ gs

on (EventKey (SpecialKey KeyDown) Down _ _) gs = return $ player.direction .~ DirDown $ gs

on (EventKey (SpecialKey KeySpace) Down _ _) gs = return gameState

on _ gs = return $ player.direction .~ Standing $ gs

stepGame _ gs@(LevelComplete _ _) = return gs
stepGame _ gs_ = do
    gs <- case view direction (_player gs_) of
            Standing -> return gs_
            DirLeft  -> maybeMove leftTile gs_ $ player.x -~ tileSize $ x +~ tileSize $ gs_
            DirRight -> maybeMove rightTile gs_ $ player.x +~ tileSize $ x -~ tileSize $ gs_
            DirUp    -> maybeMove upTile gs_ $ player.y +~ tileSize $ y -~ tileSize $ gs_
            DirDown  -> maybeMove downTile gs_ $ player.y -~ tileSize $ y +~ tileSize $ gs_
    let playerIx = currentIdx gs
    let attrs_ = ((gs ^. tiles) !! playerIx) ^. attrs
    let resetTile i = tiles.(ix i) .~ (Empty attrs_) $ gs
    case currentTile gs of
      Chip _ -> do
        playSound (soundDir ++ "collect_chip.wav") False
        return $ resetTile playerIx
      Gate _ -> return $ resetTile playerIx
      KeyYellow _ -> return $ yellowKeyCount +~ 1 $ resetTile playerIx
      KeyBlue _ -> return $ blueKeyCount +~ 1 $ resetTile playerIx
      KeyGreen _ -> return $ hasGreenKey .~ True $ resetTile playerIx
      KeyRed _ -> return $ redKeyCount +~ 1 $ resetTile playerIx
      LockYellow _ -> do
        playSound (soundDir ++ "door.wav") False
        return $ yellowKeyCount -~ 1 $ resetTile playerIx
      LockBlue _ -> do
        playSound (soundDir ++ "door.wav") False
        return $ blueKeyCount -~ 1 $ resetTile playerIx
      LockGreen _ -> do
        playSound (soundDir ++ "door.wav") False
        return $ resetTile playerIx
      LockRed _ -> do
        playSound (soundDir ++ "door.wav") False
        return $ redKeyCount -~ 1 $ resetTile playerIx
      GateFinal _ -> do
        let lvl = _level gs
        return $ LevelComplete lvl def
      _ -> return gs
