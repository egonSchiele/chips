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

levelNames = [
  "LESSON 1",
  "LESSON 2",
  "LESSON 3",
  "LESSON 4",
  "LESSON 5",
  "LESSON 6",
  "LESSON 7",
  "LESSON 8",
  "NUTS AND BOLTS",
  "BRUSHFIRE",
  "TRINITY",
  "HUNT",
  "SOUTHPOLE",
  "TELEBLOCK",
  "ELEMENTARY",
  "CELLBLOCKED",
  "NICE DAY",
  "CASTLE MOAT",
  "DIGGER",
  "TOSSED SALAD"
  ]

passwords = [
  "BDHP",
  "JXMJ",
  "ECBQ",
  "YMCJ",
  "TQKB",
  "WNLD",
  "FXQO",
  "NHAG",
  "KCRE",
  "UVWS",
  "CNPE",
  "WVHI",
  "OCKS"
  ]


oof :: GameMonad ()
oof = liftIO $ playSound (soundDir ++ "oof.wav") False

bummer :: GameMonad ()
bummer = liftIO $ playSound (soundDir ++ "bummer.wav") False

die :: GameMonad ()
die = do
  gs <- get
  when (not $ gs ^. godMode) $ do
    bummer >> put (gameState (gs ^. level))

-- given a number, returns the 2-d array that represents the tilemap for
-- that level.
tileMap :: Int -> IO [[Int]]
tileMap i = do
    contents <- B.readFile $ "maps/" ++ (show i) ++ ".json"
    let decoded = eitherDecode contents :: Either String [[Int]]
    case decoded of
      Left err -> error err
      Right map -> return map

boardW = 32 -- length . head $ tileMap
boardH = 32 -- length tileMap

playerCoords :: GameState -> (Int, Int)
playerCoords gs = ((floor (p ^. x)) // tileSize, (((boardH * tileSize) - (floor (p ^. y))) // tileSize)-1)
    where p = _player gs
          ts = _tiles gs

currentIdx :: GameState -> Int
currentIdx gs = y_ * boardW + x_
    where (x_,y_) = playerCoords gs

-- given a tile position, gives you the index
-- of that tile in the tiles array.
tilePosToIndex :: TilePos -> GameMonad Int
tilePosToIndex pos = do
    gs <- get
    let playerIdx = currentIdx gs
    return $ case pos of
      Current   -> playerIdx
      TileLeft  -> playerIdx - 1
      TileRight -> playerIdx + 1
      TileAbove -> playerIdx - boardW
      TileBelow -> playerIdx + boardW
      Arbitrary i -> i

tilePosToTile :: TilePos -> GameMonad Tile
tilePosToTile pos = do
    gs <- get
    i <- tilePosToIndex pos
    return $ (gs ^. tiles) !! i

setTile :: TilePos -> Tile -> GameMonad ()
setTile pos tile = do
    gs <- get
    i <- tilePosToIndex pos
    -- TODO refactor this -v
    let attrs_ = ((gs ^. tiles) !! i) ^. attrs
    tiles.(ix i) .= (attrs .~ attrs_ $ tile)

-- Given a tilemap (gotten with the `tileMap` function),
-- returns a list of all the tiles
renderedTiles :: [[Int]] -> [Tile]
renderedTiles tmap = renderTileMap tmap f (tileSize, tileSize)
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
          f 16 = Bee DirUp def
          f 17 = Bomb def
          f 18 = FFDown def
          f 19 = FFLeft def
          f 20 = FFRight def
          f 21 = FFUp def
          f 22 = FFRandom def
          f 23 = FFShoes def
          f 24 = FireBoots def
          f 25 = Fire def
          f 26 = Flippers def
          f 27 = Frog DirUp def
          f 28 = IceBottomLeft def
          f 29 = IceBottomRight def
          f 30 = IceSkates def
          f 31 = IceTopLeft def
          f 32 = IceTopRight def
          f 33 = Ice def
          f 34 = Sand False def
          f 35 = Spy def
          f 36 = Tank DirUp def
          f 37 = WaterSplash def
          f 38 = Water def
          f 39 = Worm DirUp def

-- Given a level number, returns the starting game state for that level
gameState :: Int -> GameState
gameState i = x .~ startX $ y .~ startY $ gs
  where player_ = (Player Standing (Empty def) def)
        tmap = unsafePerformIO $ tileMap i
        gs = GameState
              (renderedTiles tmap)
              (x .~ ((fst chipStart)*tileSize) $ y .~ ((snd chipStart)*tileSize) $ player_)
              1
              0 0 0 False
              False False False False False
              False
              def
        startX = (4 - fst chipStart) * tileSize -- "4" is (9 - 1)/2. 9 is the width of the game screen
        startY = (4 - snd chipStart) * tileSize
        -- | (x, y) of chip's start position (marked as a 0 on the tile map)
        chipStart :: (Float, Float)
        chipStart =
          case findIndex (\xs -> 0 `elem` xs) tmap of
            Nothing -> error "You need to mark where chip will stand in the tilemap. Mark it with a zero (0)."
            Just y -> (fromIntegral . fromJust $ findIndex (==0) (tmap !! y), boardH - 1 - (fromIntegral y))

moveBees :: GameMonad ()
moveBees = do
    gs <- get
    forM_ (withIndices (gs ^. tiles)) $ \(tile, i) -> do
      case tile of
        Bee _ _ -> moveBee i
        _       -> return True
    return ()

-- Move this bee counter-clockwise around an object.
moveBee :: Int -> GameMonad Bool
moveBee i = do
    gs <- get
    let bee = (gs ^. tiles) !! i
        goLeft  = moveIfEmpty (i - 1) DirLeft
        goRight = moveIfEmpty (i + 1) DirRight
        goUp    = moveIfEmpty (i - boardW) DirUp
        goDown  = moveIfEmpty (i + boardW) DirDown
        moveIfEmpty moveI dir =
          case (gs ^. tiles) !! moveI of
            Empty _ -> do
              setTile (Arbitrary i) (Empty def)
              setTile (Arbitrary moveI) (Bee dir def)
              return True
            _ -> return False
    case _beeDirection bee of
      DirUp    -> goLeft  <||> goUp    <||> goRight <||> goDown
      DirLeft  -> goDown  <||> goLeft  <||> goUp    <||> goRight
      DirDown  -> goRight <||> goDown  <||> goLeft  <||> goUp
      DirRight -> goUp    <||> goRight <||> goDown  <||> goLeft

(<||>) :: GameMonad Bool -> GameMonad Bool -> GameMonad Bool
a <||> b = do
  res <- a
  if res
    then return True
    else b
