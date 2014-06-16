{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Chips.Core where
import Chips.Types
import Chips.Utils
import Chips.Common
import qualified Data.ByteString.Lazy as B
import Chips.Globals
import Chips.RenderedTiles

eachTile :: ((Tile, Int) -> GameMonad ()) -> GameMonad ()
eachTile action = do
    gs <- get
    forM_ (withIndices (gs ^. tiles)) action
    

chipsLeft gs = length $ filter isChip (_tiles gs)
  where isChip (Chip _) = True
        isChip _        = False

oof :: GameMonad ()
oof = liftIO $ playSound (soundDir ++ "oof.wav") False

win :: GameMonad ()
win = liftIO $ playSound (soundDir ++ "win.wav") False

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
      Right tmap -> return tmap

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
      Coords (x, y) -> (x - 1) + boardW * (y - 1)

posToIndex :: TilePos -> GameState -> Int
posToIndex pos gs = 
  case pos of
    Current   -> playerIdx
    TileLeft  -> playerIdx - 1
    TileRight -> playerIdx + 1
    TileAbove -> playerIdx - boardW
    TileBelow -> playerIdx + boardW
    Arbitrary i -> i
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

-- tell all the brown buttons about the traps they are responsible for.
wireTraps :: Int -> [Tile] -> [Tile]
wireTraps i tmap =
    case lookup i trapButtons of
      Nothing -> tmap
      (Just list) -> foldl func tmap list
        where func tmap_ (i,j) = setValue tmap_ i $ \val ->
                 case val of
                   ButtonBrown _ attrs_ -> case (tmap !! j) of
                     Trap _ _ -> ButtonBrown (Arbitrary j) attrs_
                     x -> error $ "item at location j: " ++ (show j) ++ " is not a Trap. It is a " ++ (show x)
                   x -> error $ "item at location i: " ++ (show i) ++ " is not a ButtonBrown. It is a " ++ (show x)

-- tell all the teleporters their teleport destinations
wireTeleporters :: Int -> [Tile] -> [Tile]
wireTeleporters i tmap =
    case lookup i teleportDestinations of
      Nothing -> tmap
      (Just list) -> foldl func tmap list
        where func tmap_ (i,u,d,l,r) = setValue tmap_ i $ \val ->
                 case val of
                   Teleporter _ _ _ _ attrs_ -> Teleporter (Arbitrary u) (Arbitrary d) (Arbitrary l) (Arbitrary r) attrs_
                   x -> error $ "item at location i: " ++ (show i) ++ " is not a Teleporter It is a " ++ (show x)

-- Given a level number, returns the starting game state for that level
gameState :: Int -> GameState
gameState i = x .~ startX $ y .~ startY $ gs
  where player_ = (Player Standing (Empty def) def)
        tmap = unsafePerformIO $ tileMap i
        finalMap = (wireTeleporters i) . (wireTraps i) . renderedTiles $ tmap
        gs = GameState
              finalMap
              (x .~ ((fst chipStart)*tileSize) $ y .~ ((snd chipStart)*tileSize) $ player_)
              i
              0 0 0 False
              False False False False False
              False
              False
              def
        startX = if fst chipStart >= 4
                   then (4 - fst chipStart) * tileSize -- "4" is (9 - 1)/2. 9 is the width of the game screen
                   else 0

        startY = if snd chipStart <= (boardH-4)
                   then (4 - snd chipStart) * tileSize
                   else (9-boardH) * tileSize
        -- | (x, y) of chip's start position (marked as a 0 on the tile map)
        chipStart :: (Float, Float)
        chipStart =
          case findIndex (\xs -> 0 `elem` xs) tmap of
            Nothing -> error "You need to mark where chip will stand in the tilemap. Mark it with a zero (0)."
            Just y -> (fromIntegral . fromJust $ findIndex (==0) (tmap !! y), boardH - 1 - (fromIntegral y))

isButton (ButtonRed _)   = True
isButton (ButtonBlue _)  = True
isButton (ButtonGreen _) = True
isButton (ButtonBrown _ _) = True
isButton _               = False

-- move a tile from one location to another. Generally
-- used to move tanks, frogs, other enemies. If the enemy
-- had a special tile under them (like a button, for example),
-- that special tile won't be erased.
-- When the item gets moved to a location with a button,
-- that button will get pressed. If there's a trap there,
-- the item will now be in the trap.
moveTile :: Int -> Int -> Maybe Direction -> GameMonad Bool
moveTile from to newDir = do
  gs <- get
  let fromTile = (gs ^. tiles) !! from
      toTile = (gs ^. tiles) !! to
      newToTile = case toTile of
                    Trap _ _ -> tileUnder .~ fromTile $ toTile
                    _ -> case newDir of
                           Just dir_ -> dir .~ dir_ $ tileUnder .~ toTile $ fromTile
                           Nothing -> tileUnder .~ toTile $ fromTile
  tileAt (Arbitrary from) .= (fromTile ^. tileUnder)
  tileAt (Arbitrary to) .= newToTile
  when (isButton toTile) $ checkCurTile toTile
  return True

-- given a tile and a direction, move it in that direction
-- if there are no walls or anything. Used to move enemies.
-- The last param is a function that takes a tile type and
-- returns the appropriate action. This is because some
-- enemies respond differently to different tiles.
maybeMoveTile :: Int -> Direction -> Maybe ((Tile, Int) -> GameMonad Bool) -> GameMonad Bool
maybeMoveTile i dir func = do
  gs <- get
  let moveIfEmpty moveI = do
        case (gs ^. tiles) !! moveI of
          Empty _ -> moveTile i moveI (Just dir)
          ButtonRed _ -> moveTile i moveI (Just dir)
          ButtonBrown _ _ -> moveTile i moveI (Just dir)
          ButtonBlue _ -> moveTile i moveI (Just dir)
          ButtonGreen _ -> moveTile i moveI (Just dir)
          ToggleDoor True _ -> moveTile i moveI (Just dir)
          Trap _ _ -> moveTile i moveI (Just dir)
          x -> case func of
                 Nothing -> return False
                 Just f -> f (x, moveI)
  case dir of
    DirLeft  -> moveIfEmpty (i - 1)
    DirRight -> moveIfEmpty (i + 1)
    DirUp    -> moveIfEmpty (i - boardW)
    DirDown  -> moveIfEmpty (i + boardW)

onTick :: GameMonad () -> GameMonad ()
onTick action = do
  gs <- get
  when (gs ^. tick) action

moveEnemies :: GameMonad ()
moveEnemies = do
  gs <- get
  onTick $ do
    eachTile $ \(tile, i) -> do
      case tile of
        Tank dir _ _   -> maybeMoveTile i dir Nothing
        Rocket dir _ _ -> moveClockwiseLong i $ Just $ \(tile, moveI) ->
                            case tile of
                              Bomb _ -> do
                                moveTile i moveI Nothing
                                tileAt (Arbitrary moveI) .= (Empty def)
                                return True
                              _ -> return False
        BallPink dir tileUnder _ -> maybeMoveTile i dir $ Just $ \_ -> do
                                      tileAt (Arbitrary i) .= (BallPink (opposite dir) tileUnder def)
                                      return True
        Fireball dir _ _ -> moveClockwiseLong i $ Just $ \(tile, moveI) ->
                              case tile of
                                Fire _ -> moveTile i moveI (Just dir)
                                Water _ -> tileAt (Arbitrary i) .= (Empty def) >> return True
                                Ice _ -> moveTile i moveI (Just dir)
                                _ -> return False
        Bee _ _ _ -> moveClockwise i Nothing
        _       -> return False
      return ()

-- Move this bee counter-clockwise around an object.
moveClockwise :: Int -> Maybe ((Tile, Int) -> GameMonad Bool) -> GameMonad Bool
moveClockwise i func = do
    gs <- get
    let enemy = (gs ^. tiles) !! i
        goLeft  = maybeMoveTile i DirLeft func
        goRight = maybeMoveTile i DirRight func
        goUp    = maybeMoveTile i DirUp func
        goDown  = maybeMoveTile i DirDown func
    case getDirection enemy of
      Just DirUp    -> goLeft  <||> goUp    <||> goRight <||> goDown
      Just DirLeft  -> goDown  <||> goLeft  <||> goUp    <||> goRight
      Just DirDown  -> goRight <||> goDown  <||> goLeft  <||> goUp
      Just DirRight -> goUp    <||> goRight <||> goDown  <||> goLeft
      Nothing       -> error $ "don't know how to get direction for enemy: " ++ (show enemy)

-- move clockwise, but not around an object...just keep going as far as
-- you can, and when you hit a wall, turn.
moveClockwiseLong :: Int -> Maybe ((Tile, Int) -> GameMonad Bool) -> GameMonad Bool
moveClockwiseLong i func = do
    gs <- get
    let enemy = (gs ^. tiles) !! i
        goLeft  = maybeMoveTile i DirLeft func
        goRight = maybeMoveTile i DirRight func
        goUp    = maybeMoveTile i DirUp func
        goDown  = maybeMoveTile i DirDown func
    case getDirection enemy of
      Just DirUp    -> goUp    <||> goLeft  <||> goRight <||> goDown
      Just DirLeft  -> goLeft  <||> goDown  <||> goUp    <||> goRight
      Just DirDown  -> goDown  <||> goRight  <||> goLeft  <||> goUp
      Just DirRight -> goRight <||> goUp    <||> goDown  <||> goLeft
      Nothing       -> error $ "don't know how to get direction for enemy: " ++ (show enemy)

(<||>) :: GameMonad Bool -> GameMonad Bool -> GameMonad Bool
a <||> b = do
  res <- a
  if res
    then return True
    else b

opposite DirUp = DirDown
opposite DirDown = DirUp
opposite DirLeft = DirRight
opposite DirRight = DirLeft

checkCurTile :: Tile -> GameMonad ()
checkCurTile (Chip _) = do
  liftIO $ playSound (soundDir ++ "collect_chip.wav") False
  tileAt Current .= (Empty def)
checkCurTile (Gate _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  tileAt Current .= (Empty def)
checkCurTile (KeyYellow _) = do
  yellowKeyCount += 1
  tileAt Current .= (Empty def)
checkCurTile (KeyBlue _) = do
  blueKeyCount += 1
  tileAt Current .= (Empty def)
checkCurTile (KeyGreen _) = do
  hasGreenKey .= True
  tileAt Current .= (Empty def)
checkCurTile (KeyRed _) = do
  redKeyCount += 1
  tileAt Current .= (Empty def)
checkCurTile (LockYellow _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  yellowKeyCount -= 1
  tileAt Current .= (Empty def)
checkCurTile (LockBlue _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  blueKeyCount -= 1
  tileAt Current .= (Empty def)
checkCurTile (LockGreen _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  tileAt Current .= (Empty def)
checkCurTile (LockRed _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  redKeyCount -= 1
  tileAt Current .= (Empty def)
checkCurTile (GateFinal _) = do
  win
  gs <- get
  put $ gameState (gs ^. level + 1)
checkCurTile (Water _) = do
  gs <- get
  when (not . _hasFlippers $ gs) die
checkCurTile (Fire _) = do
  gs <- get
  when (not . _hasFireBoots $ gs) die
checkCurTile (Ice _) = do
  gs <- get
  let bounceCheck pos dir = do
        tile <- use $ tileAt pos
        -- TODO:
        -- major code duplication between this and maybeMove function,
        -- and this function doesn't even take care of all the cases
        -- that that function does!
        case tile of
          Wall _ -> player.direction .= dir
          BlueWall True _ -> do
            tileAt pos .= (Wall def)
            player.direction .= dir
          BlueWall False _ -> do
            tileAt pos .= (Empty def)
          InvisibleWall True _ -> do
            tileAt pos .= (Wall def)
            player.direction .= dir
          InvisibleWall False _ -> player.direction .= dir
          _ -> return ()
  case gs ^. player.direction of
    DirUp -> bounceCheck TileAbove DirDown
    DirDown -> bounceCheck TileBelow DirUp
    DirLeft -> bounceCheck TileLeft DirRight
    DirRight -> bounceCheck TileRight DirLeft
    _ -> return ()

checkCurTile (IceBottomLeft _) = do
  gs <- get
  when (not $ _hasIceSkates gs || _godMode gs) $ do
    case gs ^. player.direction of
      DirLeft -> player.direction .= DirUp
      DirDown -> player.direction .= DirRight
      _ -> return ()
checkCurTile (IceTopLeft _) = do
  gs <- get
  when (not $ _hasIceSkates gs || _godMode gs) $ do
    case gs ^. player.direction of
      DirLeft -> player.direction .= DirDown
      DirUp   -> player.direction .= DirRight
      _ -> return ()
checkCurTile (IceTopRight _) = do
  gs <- get
  when (not $ _hasIceSkates gs || _godMode gs) $ do
    case gs ^. player.direction of
      DirRight -> player.direction .= DirDown
      DirUp    -> player.direction .= DirLeft
      _ -> return ()
checkCurTile (IceBottomRight _) = do
  gs <- get
  when (not $ _hasIceSkates gs || _godMode gs) $ do
    case gs ^. player.direction of
      DirRight -> player.direction .= DirUp
      DirDown  -> player.direction .= DirLeft
      _ -> return ()
checkCurTile (FFLeft _) = do
  gs <- get
  onTick $ do
    when (not $ _hasFFShoes gs || _godMode gs) $ do
      player.x -= tileSize
      x += tileSize
checkCurTile (FFRight _) = do
  gs <- get
  onTick $ do
    when (not $ _hasFFShoes gs || _godMode gs) $ do
      player.x += tileSize
      x -= tileSize
checkCurTile (FFUp _) = do
  gs <- get
  onTick $ do
    when (not $ _hasFFShoes gs || _godMode gs) $ do
      player.y += tileSize
      y -= tileSize
checkCurTile (FFDown _) = do
  gs <- get
  onTick $ do
    when (not $ _hasFFShoes gs || _godMode gs) $ do
      player.y -= tileSize
      y += tileSize
checkCurTile (FFShoes _) = do
  hasFFShoes .= True
  tileAt Current .= Empty def
checkCurTile (FireBoots _) = do
  hasFireBoots .= True
  tileAt Current .= Empty def
checkCurTile (Flippers _) = do
  hasFlippers .= True
  tileAt Current .= Empty def
checkCurTile (IceSkates _) = do
  hasIceSkates .= True
  tileAt Current .= Empty def
checkCurTile (Sand (Water _) _) = tileAt Current .= Empty def
checkCurTile (Sand _ _) = do
  gs <- get
  case gs ^. player.direction of
    Standing -> error "standing on sand?"
    DirLeft  -> moveSand TileLeft
    DirRight -> moveSand TileRight
    DirUp    -> moveSand TileAbove
    DirDown  -> moveSand TileBelow
checkCurTile (ButtonGreen _) = do
  gs <- get
  eachTile $ \(tile, i) -> do
    case tile of
      ToggleDoor x _ -> tileAt (Arbitrary i) .= ToggleDoor (not x) def
      _       -> return ()
checkCurTile (ButtonBlue _) = do
  gs <- get
  eachTile $ \(tile, i) -> do
    case tile of
      Tank dir tileUnder _ -> tileAt (Arbitrary i) .= Tank (opposite dir) tileUnder def
      _       -> return ()
checkCurTile (ButtonRed _) = do
  gs <- get
  eachTile $ \(tile, i) -> do
    case tile of
      GeneratorFireball dir _ -> do
        let genAt loc = do
              let oldTile = (gs ^. tiles) !! loc
              tileAt (Arbitrary loc) .= Fireball dir oldTile def
        case dir of
          DirLeft  -> genAt (i - 1)
          DirRight -> genAt (i + 1)
          DirUp    -> genAt (i - boardW)
          DirDown  -> genAt (i + boardW)
      _       -> return ()
  return ()
checkCurTile (Trap inTrap _) = do
  gs <- get
  when (not $ gs ^. godMode) $ do
    case inTrap of
      Empty _ -> do
        tileAt Current .= Trap (PlayerInTrap def) def
        player.direction .= Standing
        disableInput .= True
      PlayerInTrap _ -> return ()
      x -> die
checkCurTile (ButtonBrown trapPos _) = do
  gs <- get
  i <- tilePosToIndex trapPos
  case (gs ^. tiles) !! i of
    Trap t _ ->
      case t of
        -- free the enemy
        Rocket dir _ _ -> tileAt (Arbitrary i) .= Rocket dir (Trap (Empty def) def) def
        Fireball dir _ _ -> tileAt (Arbitrary i) .= Fireball dir (Trap (Empty def) def) def
        Bee dir _ _ -> tileAt (Arbitrary i) .= Bee dir (Trap (Empty def) def) def
        Frog dir _ _ -> tileAt (Arbitrary i) .= Frog dir (Trap (Empty def) def) def
        Tank dir _ _ -> tileAt (Arbitrary i) .= Tank dir (Trap (Empty def) def) def
        Worm dir _ _ -> tileAt (Arbitrary i) .= Worm dir (Trap (Empty def) def) def
        BallPink dir _ _ -> tileAt (Arbitrary i) .= BallPink dir (Trap (Empty def) def) def
        PlayerInTrap _ -> do
          disableInput .= False
        _ -> return ()
    _ -> return ()
checkCurTile (Bee _ _ _) = die
checkCurTile (Frog _ _ _) = die
checkCurTile (Tank _ _ _) = die
checkCurTile (Worm _ _ _) = die
checkCurTile (Bomb _) = die
checkCurTile (BallPink _ _ _) = die
checkCurTile (Rocket _ _ _) = die
checkCurTile (Fireball _ _ _) = die
checkCurTile (Teleporter u d l r _) = do
  gs <- get
  case gs ^. player.direction of
    DirUp -> movePlayer u
    DirDown -> movePlayer d
    DirLeft -> movePlayer l
    DirRight -> movePlayer r
    Standing -> return ()
checkCurTile (Spy _) = do
  gs <- get
  when (not $ gs ^. godMode) $ do
    redKeyCount .= 0
    blueKeyCount .= 0
    yellowKeyCount .= 0
    hasGreenKey .= False
    hasFFShoes .= False
    hasFireBoots .= False
    hasFlippers .= False
    hasIceSkates .= False
checkCurTile (Dirt _) = tileAt Current .= Empty def
checkCurTile _ = return ()

-- moveSand :: Int -> GameState -> IO GameState
moveSand destPos = do
    destTile <- use $ tileAt destPos
    curTile <- use $ tileAt Current
    let isBomb (Bomb _) = True
        isBomb _        = False
    case curTile of
      Sand t _ -> do
        tileAt Current .= t
        tileAt destPos .= Sand destTile def
        player.standingOn .= t
        checkCurTile t
        when (isButton destTile) $ checkCurTile destTile
        when (isBomb destTile) $ tileAt destPos .= Empty def
      _ -> error "current tile isn't a sand tile. How did you get here?"

movePlayer :: TilePos -> GameMonad ()
movePlayer pos = do
  gs <- get
  destTile <- use $ tileAt pos
  let curTile = gs ^. player.standingOn
      diffX = (destTile ^. x) - (curTile ^. x)
      diffY = (destTile ^. y) - (curTile ^. y)
  player.x += diffX
  player.y += diffY
  x -= diffX
  y -= diffY
  liftIO $ print (diffX, diffY)

once :: GameMonad () -> GameMonad ()
once action = do
    cur <- liftIO . readIORef $ curLocation
    prev <- liftIO . readIORef $ prevLocation
    when (cur /= prev) action

closeRecessedWall :: GameMonad ()
closeRecessedWall = do
  gs <- get
  case gs ^. player.standingOn of
    RecessedWall _ -> tileAt Current .= Wall def
    _ -> return ()
