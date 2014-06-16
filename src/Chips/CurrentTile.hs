module Chips.CurrentTile where
import Chips.Types
import Chips.Utils
import Chips.Imports
import Chips.Globals
import Chips.GameState
import Chips.Position

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
  lvl <- use level
  put $ gameState (lvl + 1)
checkCurTile (Water _) = do
  guardGodMode $ whenM not hasFlippers die
checkCurTile (Fire _) = do
  guardGodMode $ whenM not hasFireBoots die
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
  guardGodMode $ do
    whenM not hasIceSkates $ do
      case gs ^. player.direction of
        DirLeft -> player.direction .= DirUp
        DirDown -> player.direction .= DirRight
        _ -> return ()

checkCurTile (IceTopLeft _) = do
  gs <- get
  guardGodMode $ do
    whenM not hasIceSkates $ do
      case gs ^. player.direction of
        DirLeft -> player.direction .= DirDown
        DirUp   -> player.direction .= DirRight
        _ -> return ()
checkCurTile (IceTopRight _) = do
  gs <- get
  guardGodMode $ do
    whenM not hasIceSkates $ do
      case gs ^. player.direction of
        DirRight -> player.direction .= DirDown
        DirUp    -> player.direction .= DirLeft
        _ -> return ()
checkCurTile (IceBottomRight _) = do
  gs <- get
  guardGodMode $ do
    whenM not hasIceSkates $ do
      case gs ^. player.direction of
        DirRight -> player.direction .= DirUp
        DirDown  -> player.direction .= DirLeft
        _ -> return ()
checkCurTile (FFLeft _) = do
  onTick $ do
    guardGodMode $ do
      whenM not hasFFShoes $ do
        player.x -= tileSize
        x += tileSize
checkCurTile (FFRight _) = do
  onTick $ do
    guardGodMode $ do
      whenM not hasFFShoes $ do
        player.x += tileSize
        x -= tileSize
checkCurTile (FFUp _) = do
  onTick $ do
    guardGodMode $ do
      whenM not hasFFShoes $ do
        player.y += tileSize
        y -= tileSize
checkCurTile (FFDown _) = do
  onTick $ do
    guardGodMode $ do
      whenM not hasFFShoes $ do
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
  eachTile $ \(tile, i) -> do
    case tile of
      ToggleDoor x _ -> tileAt (Ix i) .= ToggleDoor (not x) def
      _       -> return ()
checkCurTile (ButtonBlue _) = do
  eachTile $ \(tile, i) -> do
    case tile of
      Tank dir tileUnder _ -> tileAt (Ix i) .= Tank (opposite dir) tileUnder def
      _       -> return ()
checkCurTile (ButtonRed _) = do
  eachTile $ \(tile, i) -> do
    case tile of
      GeneratorFireball dir _ -> do
        let genAt loc = do
              oldTile <- use $ tileAt (Ix loc)
              tileAt (Ix loc) .= Fireball dir oldTile def
        case dir of
          DirLeft  -> genAt (i - 1)
          DirRight -> genAt (i + 1)
          DirUp    -> genAt (i - boardW)
          DirDown  -> genAt (i + boardW)
      _       -> return ()
  return ()
checkCurTile (Trap (Empty _) _) = do
  guardGodMode $ do
    tileAt Current .= Trap (PlayerInTrap def) def
    player.direction .= Standing
    disableInput .= True
    
-- just means you're already in the trap
checkCurTile (Trap (PlayerInTrap _) _) = return ()

-- assume there is some enemy in the trap
checkCurTile (Trap _ _) = die
checkCurTile (ButtonBrown trapPos _) = do
  gs <- get
  i <- tilePosToIndex trapPos
  let trap = (Trap (Empty def) def)
  case gs ^. tiles.(idx i) of
    Trap t _ ->
      case t of
        -- free the enemy
        Rocket dir _ _   -> tileAt (Ix i) .= Rocket dir trap def
        Fireball dir _ _ -> tileAt (Ix i) .= Fireball dir trap def
        Bee dir _ _      -> tileAt (Ix i) .= Bee dir trap def
        Frog dir _ _     -> tileAt (Ix i) .= Frog dir trap def
        Tank dir _ _     -> tileAt (Ix i) .= Tank dir trap def
        Worm dir _ _     -> tileAt (Ix i) .= Worm dir trap def
        BallPink dir _ _ -> tileAt (Ix i) .= BallPink dir trap def
        PlayerInTrap _   -> disableInput .= False
        _                -> return ()
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
    DirUp    -> movePlayer u
    DirDown  -> movePlayer d
    DirLeft  -> movePlayer l
    DirRight -> movePlayer r
    Standing -> return ()
checkCurTile (Spy _) = do
  gs <- get
  guardGodMode $ do
    redKeyCount    .= 0
    blueKeyCount   .= 0
    yellowKeyCount .= 0
    hasGreenKey    .= False
    hasFFShoes     .= False
    hasFireBoots   .= False
    hasFlippers    .= False
    hasIceSkates   .= False
checkCurTile (Dirt _) = tileAt Current .= Empty def
checkCurTile _ = return ()

-- moveSand :: Int -> GameState -> IO GameState
moveSand destPos = do
    destTile <- use $ tileAt destPos
    curTile  <- use $ tileAt Current
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
  curTile <- use $ tileAt Current
  let diffX = (destTile ^. x) - (curTile ^. x)
      diffY = (destTile ^. y) - (curTile ^. y)
  player.x += diffX
  player.y += diffY
  x -= diffX
  y -= diffY

die :: GameMonad ()
die = do
  gs <- get
  guardGodMode $ do
    bummer >> put (gameState (gs ^. level))
