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
    let inWater = case destTile of
                    Water _ -> True
                    _ -> False
    setTile Current (Empty def)
    setTile destPos (Sand inWater def)
