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
import Data.IORef
import System.IO.Unsafe
import Data.Time.Clock
import Data.Aeson
import qualified Data.ByteString.Lazy as B

tileSize = 32
soundDir = "sounds/"

oof :: IO ()
oof = playSound (soundDir ++ "oof.wav") False

tileMap :: [[Int]]
tileMap = unsafePerformIO $ do
    contents <- B.readFile "maps/01.json"
    let decoded = eitherDecode contents :: Either String [[Int]]
    case decoded of
      Left err -> error err
      Right map -> return map

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

gameState = x .~ startX $ y .~ startY $ gs
  where player_ = (Player Standing def)
        gs = GameState renderedTiles (x .~ ((fst chipStart)*tileSize) $ y .~ ((snd chipStart)*tileSize) $ player_) 1 "LESSON 1" "BDHP" 0 0 0 False def
        startX = (4 - fst chipStart) * tileSize -- "4" is (9 - 1)/2. 9 is the width of the game screen
        startY = (4 - snd chipStart) * tileSize

main = do
    print chipStart
    playSound (soundDir ++ "chips01.wav") True
    run "chips challenge" (9 * tileSize, 9 * tileSize) gameState on stepGame

chipsLeft gs = length $ filter isChip (_tiles gs)
  where isChip (Chip _) = True
        isChip _        = False

-- this keeps track of when we last moved.
-- So if the user is holding a key down, we
-- don't want to move too fast.
lastPress :: IORef UTCTime
lastPress = unsafePerformIO $ do
  now <- getCurrentTime
  newIORef now

($=) ref val = modifyIORef ref (const val)

-- if a user is holding a key down, move
-- this fast (currently every 1/4 of a second)
moveSpeed = -0.25

maybeMove :: (GameState -> Tile) -> GameState -> GameState -> IO GameState
maybeMove func gs newGs = do
    cur <- getCurrentTime
    last <- readIORef lastPress
    if diffUTCTime last cur > moveSpeed
      then return gs
      else do
        lastPress $= cur
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

resetMoveTime = modifyIORef lastPress (addUTCTime moveSpeed)

on (EventKey (SpecialKey KeyLeft) Down _ _) gs = do
    resetMoveTime
    return $ player.direction .~ DirLeft $ gs

on (EventKey (SpecialKey KeyRight) Down _ _) gs = do
    resetMoveTime
    return $ player.direction .~ DirRight $ gs

on (EventKey (SpecialKey KeyUp) Down _ _) gs = do
    resetMoveTime
    return $ player.direction .~ DirUp $ gs

on (EventKey (SpecialKey KeyDown) Down _ _) gs = do
    resetMoveTime
    return $ player.direction .~ DirDown $ gs

on (EventKey (SpecialKey KeySpace) Down _ _) gs = return gameState

on _ gs = return $ player.direction .~ Standing $ gs

stepGame _ gs@(LevelComplete _ _) = return gs
stepGame _ gs_ = do
    gs <- case view direction (_player gs_) of
            Standing -> return gs_
            DirLeft  -> maybeMove leftTile gs_  $ player.x -~ tileSize $ x +~ tileSize $ gs_
            DirRight -> maybeMove rightTile gs_ $ player.x +~ tileSize $ x -~ tileSize $ gs_
            DirUp    -> maybeMove upTile gs_    $ player.y +~ tileSize $ y -~ tileSize $ gs_
            DirDown  -> maybeMove downTile gs_  $ player.y -~ tileSize $ y +~ tileSize $ gs_
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
