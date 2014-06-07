{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import Chips

main = do
    print chipStart
    playSound (soundDir ++ "chips01.wav") True
                                                                    -- move this logic to ActionKid instead
    run "chips challenge" (9 * tileSize, 9 * tileSize) gameState on (\_ gs -> execStateT stepGame gs)

stepGame :: GameMonad ()
stepGame = do
  gs <- get
  maybeDisableInput
  case gs ^. player.direction of
    DirLeft  -> do
      maybeMove TileLeft $ do
        player.x -= tileSize
        x += tileSize
    DirRight -> do
      maybeMove TileRight $ do
        player.x += tileSize
        x -= tileSize
    DirUp    -> do
      maybeMove TileAbove $ do
        player.y += tileSize
        y -= tileSize
    DirDown  -> do
      maybeMove TileBelow $ do
        player.y -= tileSize
        y += tileSize
    _ -> return ()
  curTile <- tilePosToTile Current
  checkCurTile curTile

maybeDisableInput = do
  curTile <- tilePosToTile Current
  gs <- get    
  case curTile of
    Ice            _ -> do
      when (not $ gs ^. hasIceSkates) $
        disableInput .= True
    IceBottomLeft  _ -> do
      when (not $ gs ^. hasIceSkates) $
        disableInput .= True
    IceBottomRight _ -> do
      when (not $ gs ^. hasIceSkates) $
        disableInput .= True
    IceTopLeft     _ -> do
      when (not $ gs ^. hasIceSkates) $
        disableInput .= True
    IceTopRight    _ -> do
      when (not $ gs ^. hasIceSkates) $
        disableInput .= True
    FFLeft         _ -> do
      when (not $ gs ^. hasFFShoes) $
        disableInput .= True
    FFRight        _ -> do
      when (not $ gs ^. hasFFShoes) $
        disableInput .= True
    FFUp           _ -> do
      when (not $ gs ^. hasFFShoes) $
        disableInput .= True
    FFDown         _ -> do
      when (not $ gs ^. hasFFShoes) $
        disableInput .= True
    _                -> do
      when (gs ^. disableInput) $ do
        player.direction .= Standing
        disableInput .= False
    

checkCurTile :: Tile -> GameMonad ()
checkCurTile (Chip _) = do
  liftIO $ playSound (soundDir ++ "collect_chip.wav") False
  setTile Current (Empty def)
checkCurTile (Gate _) = setTile Current (Empty def)
checkCurTile (KeyYellow _) = do
  yellowKeyCount += 1
  setTile Current (Empty def)
checkCurTile (KeyBlue _) = do
  blueKeyCount += 1
  setTile Current (Empty def)
checkCurTile (KeyGreen _) = do
  hasGreenKey .= True
  setTile Current (Empty def)
checkCurTile (KeyRed _) = do
  redKeyCount += 1
  setTile Current (Empty def)
checkCurTile (LockYellow _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  yellowKeyCount -= 1
  setTile Current (Empty def)
checkCurTile (LockBlue _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  blueKeyCount -= 1
  setTile Current (Empty def)
checkCurTile (LockGreen _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  setTile Current (Empty def)
checkCurTile (LockRed _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  redKeyCount -= 1
  setTile Current (Empty def)
checkCurTile (GateFinal _) = do
  -- let lvl = _level gs
  return ()
  -- put $ LevelComplete lvl def
checkCurTile (Water _) = do
  gs <- get
  when (not . _hasFlippers $ gs) die
checkCurTile (Fire _) = do
  gs <- get
  when (not . _hasFireBoots $ gs) die
checkCurTile (Ice _) = do
  gs <- get
  when (not . _hasIceSkates $ gs) $ do
    case gs ^. player.direction of
      DirLeft  -> do
          player.x -= tileSize
          x += tileSize
      DirRight -> do
          player.x += tileSize
          x -= tileSize
      DirUp    -> do
          player.y += tileSize
          y -= tileSize
      DirDown  -> do
          player.y -= tileSize
          y += tileSize
      _ -> return ()
checkCurTile (IceBottomLeft _) = do
  gs <- get
  when (not . _hasIceSkates $ gs) $ do
    case gs ^. player.direction of
      DirLeft -> do
        player.direction .= DirUp
        player.y += tileSize
        y -= tileSize
      DirDown -> do
        player.direction .= DirRight
        player.x += tileSize
        x -= tileSize
      _ -> return ()
checkCurTile (IceTopLeft _) = do
  gs <- get
  when (not . _hasIceSkates $ gs) $ do
    case gs ^. player.direction of
      DirLeft -> do
        player.direction .= DirDown
        player.y -= tileSize
        y += tileSize
      DirUp -> do
        player.direction .= DirRight
        player.x += tileSize
        x -= tileSize
      _ -> return ()
checkCurTile (IceTopRight _) = do
  gs <- get
  when (not . _hasIceSkates $ gs) $ do
    case gs ^. player.direction of
      DirRight -> do
        player.direction .= DirDown
        player.y -= tileSize
        y += tileSize
      DirUp -> do
        player.direction .= DirLeft
        player.x -= tileSize
        x += tileSize
      _ -> return ()
checkCurTile (IceBottomRight _) = do
  gs <- get
  when (not . _hasIceSkates $ gs) $ do
    case gs ^. player.direction of
      DirRight -> do
        player.direction .= DirUp
        player.y += tileSize
        y -= tileSize
      DirDown -> do
        player.direction .= DirLeft
        player.x -= tileSize
        x += tileSize
      _ -> return ()
checkCurTile (FFLeft _) = do
  gs <- get
  when (not . _hasFFShoes $ gs) $ do
    player.x -= tileSize
    x += tileSize
checkCurTile (FFRight _) = do
  gs <- get
  when (not . _hasFFShoes $ gs) $ do
    player.x += tileSize
    x -= tileSize
checkCurTile (FFUp _) = do
  gs <- get
  when (not . _hasFFShoes $ gs) $ do
    player.y += tileSize
    y -= tileSize
checkCurTile (FFDown _) = do
  gs <- get
  when (not . _hasFFShoes $ gs) $ do
    player.y -= tileSize
    y += tileSize
checkCurTile (FFShoes _) = do
  hasFFShoes .= True
  setTile Current (Empty def)
checkCurTile (FireBoots _) = do
  hasFireBoots .= True
  setTile Current (Empty def)
checkCurTile (Flippers _) = do
  hasFlippers .= True
  setTile Current (Empty def)
checkCurTile (IceSkates _) = do
  hasIceSkates .= True
  setTile Current (Empty def)
checkCurTile (Sand True _) = setTile Current (Empty def)
checkCurTile (Sand False _) = do
  gs <- get
  case gs ^. player.direction of
    Standing -> error "standing on sand?"
    DirLeft  -> moveSand TileLeft
    DirRight -> moveSand TileRight
    DirUp    -> moveSand TileAbove
    DirDown  -> moveSand TileBelow


checkCurTile _ = return ()

-- moveSand :: Int -> GameState -> IO GameState
moveSand destPos = do
    destTile <- tilePosToTile destPos
    let intoWater = case destTile of
                    Water _ -> True
                    _ -> False
    setTile Current (Empty def)
    setTile destPos (Sand intoWater def)
