{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import Chips

main = do
    print chipStart
    playSound (soundDir ++ "chips01.wav") True
    run "chips challenge" (9 * tileSize, 9 * tileSize) gameState on (\_ gs -> execStateT stepGame gs)

stepGame :: GameMonad
-- stepGame _ gs@(LevelComplete _ _) = return gs
stepGame = do
    gs <- get
    case gs ^. player.direction of
      DirLeft  -> do
        maybeMove leftTile $ do
          player.x -= tileSize
          x += tileSize
      DirRight -> do
        maybeMove rightTile $ do
          player.x += tileSize
          x -= tileSize
      DirUp    -> do
        maybeMove upTile $ do
          player.y += tileSize
          y -= tileSize
      DirDown  -> do
        maybeMove downTile $ do
          player.y -= tileSize
          y += tileSize
      _ -> return ()
    checkCurTile (currentTile gs)

checkCurTile :: Tile -> GameMonad
checkCurTile (Chip _) = do
  liftIO $ playSound (soundDir ++ "collect_chip.wav") False
  resetCurrentTile
checkCurTile (Gate _) = resetCurrentTile
checkCurTile (KeyYellow _) = do
    yellowKeyCount += 1
    resetCurrentTile
checkCurTile (KeyBlue _) = do
    blueKeyCount += 1
    resetCurrentTile
checkCurTile (KeyGreen _) = do
    hasGreenKey .= True
    resetCurrentTile
checkCurTile (KeyRed _) = do
    redKeyCount += 1
    resetCurrentTile
checkCurTile (LockYellow _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  yellowKeyCount -= 1
  resetCurrentTile
checkCurTile (LockBlue _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  blueKeyCount -= 1
  resetCurrentTile
checkCurTile (LockGreen _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  resetCurrentTile
checkCurTile (LockRed _) = do
  liftIO $ playSound (soundDir ++ "door.wav") False
  redKeyCount -= 1
  resetCurrentTile
checkCurTile (GateFinal _) = do
  -- let lvl = _level gs
  return ()
  -- put $ LevelComplete lvl def
checkCurTile (Water _) = do
  gs <- get
  when (not . _hasFlippers $ gs) die
checkCurTile (Sand _ _) = do
  gs <- get
  let playerIdx = currentIdx gs
  case gs ^. player.direction of
    Standing -> error "standing on sand?"
    DirLeft  -> moveSand (playerIdx - 1)
    DirRight -> moveSand (playerIdx + 1)
    DirUp    -> moveSand (playerIdx - boardW)
    DirDown  -> moveSand (playerIdx + boardW)

checkCurTile _ = return ()

-- moveSand :: Int -> GameState -> IO GameState
moveSand destIx = do
    gs <- get
    let playerIx = currentIdx gs
        playerLocAttrs = ((gs ^. tiles) !! playerIx) ^. attrs
        destAttrs = ((gs ^. tiles) !! destIx) ^. attrs
        inWater = case (gs ^. tiles) !! destIx of
                    Water _ -> True
                    _ -> False
        alreadyInWater = case (gs ^. tiles) !! playerIx of
                           Sand True _ -> True
                           Sand False _ -> False
                           _ -> False
    if alreadyInWater
     then tiles.(ix playerIx) .= (Empty playerLocAttrs)
     else do
       tiles.(ix destIx) .= (Sand inWater destAttrs)
       tiles.(ix playerIx) .= (Empty playerLocAttrs)
