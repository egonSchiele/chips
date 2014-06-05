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

bummer :: IO ()
bummer = playSound (soundDir ++ "bummer.wav") False

die :: IO GameState
die = bummer >> return gameState

tileMap :: [[Int]]
tileMap = unsafePerformIO $ do
    contents <- B.readFile "maps/2.json"
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
          f 15 = Amoeba def
          f 16 = Bee def
          f 17 = Bomb def
          f 18 = FFDown def
          f 19 = FFLeft def
          f 20 = FFRight def
          f 21 = FFUp def
          f 22 = FFRandom def
          f 23 = FFShoes def
          f 24 = FireBoots def
          f 25 = Fire def
          f 26 = Flipper def
          f 27 = Frog def
          f 28 = IceBottomLeft def
          f 29 = IceBottomRight def
          f 30 = IceSkates def
          f 31 = IceTopLeft def
          f 32 = IceTopRight def
          f 33 = Ice def
          f 34 = Sand False def
          f 35 = Spy def
          f 36 = Tank def
          f 37 = WaterSplash def
          f 38 = Water def
          f 39 = Worm def

-- | (x, y) of chip's start position (marked as a 0 on the tile map)
chipStart :: (Float, Float)
chipStart = case findIndex (\xs -> 0 `elem` xs) tileMap of
              Nothing -> error "You need to mark where chip will stand in the tilemap. Mark it with a zero (0)."
              Just y -> (fromIntegral . fromJust $ findIndex (==0) (tileMap !! y), fromIntegral $ y + 6)

gameState = x .~ startX $ y .~ startY $ gs
  where player_ = (Player Standing def)
        gs = GameState renderedTiles (x .~ ((fst chipStart)*tileSize) $ y .~ ((snd chipStart)*tileSize) $ player_) 1 "LESSON 1" "BDHP" 0 0 0 False False False False False def
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

resetCurrentTile gs = tiles.(ix i) .~ (Empty attrs_) $ gs
    where attrs_ = ((gs ^. tiles) !! i) ^. attrs
          i = currentIdx gs

stepGame _ gs@(LevelComplete _ _) = return gs
stepGame _ gs_ = do
    gs <- case view direction (_player gs_) of
            Standing -> return gs_
            DirLeft  -> maybeMove leftTile gs_  $ player.x -~ tileSize $ x +~ tileSize $ gs_
            DirRight -> maybeMove rightTile gs_ $ player.x +~ tileSize $ x -~ tileSize $ gs_
            DirUp    -> maybeMove upTile gs_    $ player.y +~ tileSize $ y -~ tileSize $ gs_
            DirDown  -> maybeMove downTile gs_  $ player.y -~ tileSize $ y +~ tileSize $ gs_
    checkCurTile gs (currentTile gs)

checkCurTile :: GameState -> Tile -> IO GameState
checkCurTile gs (Chip _) = do
  playSound (soundDir ++ "collect_chip.wav") False
  return $ resetCurrentTile gs
checkCurTile gs (Gate _) = return $ resetCurrentTile gs
checkCurTile gs (KeyYellow _) = return $ yellowKeyCount +~ 1 $ resetCurrentTile gs
checkCurTile gs (KeyBlue _) = return $ blueKeyCount +~ 1 $ resetCurrentTile gs
checkCurTile gs (KeyGreen _) = return $ hasGreenKey .~ True $ resetCurrentTile gs
checkCurTile gs (KeyRed _) = return $ redKeyCount +~ 1 $ resetCurrentTile gs
checkCurTile gs (LockYellow _) = do
  playSound (soundDir ++ "door.wav") False
  return $ yellowKeyCount -~ 1 $ resetCurrentTile gs
checkCurTile gs (LockBlue _) = do
  playSound (soundDir ++ "door.wav") False
  return $ blueKeyCount -~ 1 $ resetCurrentTile gs
checkCurTile gs (LockGreen _) = do
  playSound (soundDir ++ "door.wav") False
  return $ resetCurrentTile gs
checkCurTile gs (LockRed _) = do
  playSound (soundDir ++ "door.wav") False
  return $ redKeyCount -~ 1 $ resetCurrentTile gs
checkCurTile gs (GateFinal _) = do
  let lvl = _level gs
  return $ LevelComplete lvl def
checkCurTile gs (Water _) = do
  if (not . _hasFlippers $ gs)
    then die
    else return gs
checkCurTile gs (Sand _ _) = do
  let playerIdx = currentIdx gs
  case view direction (_player gs) of
    Standing -> error "standing on sand?"
    DirLeft  -> moveSand (playerIdx - 1) gs
    DirRight -> moveSand (playerIdx + 1) gs
    DirUp    -> moveSand (playerIdx - boardW) gs
    DirDown  -> moveSand (playerIdx + boardW) gs
checkCurTile gs _ = return gs

moveSand :: Int -> GameState -> IO GameState
moveSand destIx gs = if alreadyInWater
                       then return $ tiles.(ix playerIx) .~ (Empty playerLocAttrs) $ gs
                       else return $ tiles.(ix destIx) .~ (Sand inWater destAttrs) $
                            tiles.(ix playerIx) .~ (Empty playerLocAttrs) $ gs
    where playerIx = currentIdx gs
          playerLocAttrs = ((gs ^. tiles) !! playerIx) ^. attrs
          destAttrs = ((gs ^. tiles) !! destIx) ^. attrs
          inWater = case (gs ^. tiles) !! destIx of
                      Water _ -> True
                      _ -> False
          alreadyInWater = case (gs ^. tiles) !! playerIx of
                             Sand True _ -> True
                             Sand False _ -> False
                             _ -> False
