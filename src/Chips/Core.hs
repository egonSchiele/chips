{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Chips.Core where
import Chips.Types
import Chips.Utils
import Chips.Common
import qualified Data.ByteString.Lazy as B

chipsLeft gs = length $ filter isChip (_tiles gs)
  where isChip (Chip _) = True
        isChip _        = False

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

resetCurrentTile :: GameState -> GameState
resetCurrentTile gs = tiles.(ix i) .~ (Empty attrs_) $ gs
    where attrs_ = ((gs ^. tiles) !! i) ^. attrs
          i = currentIdx gs


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
              Just y -> (fromIntegral . fromJust $ findIndex (==0) (tileMap !! y), fromIntegral $ y + 6) -- where is 6 coming from? magic number...

gameState = x .~ startX $ y .~ startY $ gs
  where player_ = (Player Standing def)
        gs = GameState renderedTiles (x .~ ((fst chipStart)*tileSize) $ y .~ ((snd chipStart)*tileSize) $ player_) 1 "LESSON 1" "BDHP" 0 0 0 False False False False False def
        startX = (4 - fst chipStart) * tileSize -- "4" is (9 - 1)/2. 9 is the width of the game screen
        startY = (4 - snd chipStart) * tileSize
