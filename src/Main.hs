{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import Chips

main = do
    print chipStart
    playSound (soundDir ++ "chips01.wav") True
    run "chips challenge" (9 * tileSize, 9 * tileSize) gameState on (\_ gs -> execStateT stepGame gs)

-- stepGame :: Float -> GameState -> IO GameState
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

-- checkCurTile :: GameState -> Tile -> IO GameState
checkCurTile (Chip _) = do
  playSound (soundDir ++ "collect_chip.wav") False
  resetCurrentTile
checkCurTile gs (Gate _) = resetCurrentTile
checkCurTile gs (KeyYellow _) = do
    yellowKeyCount += 1
    resetCurrentTile
checkCurTile gs (KeyBlue _) = do
    blueKeyCount += 1
    resetCurrentTile
checkCurTile gs (KeyGreen _) = do
    hasGreenKey .= True
    resetCurrentTile
checkCurTile gs (KeyRed _) = do
    redKeyCount += 1
    resetCurrentTile
checkCurTile gs (LockYellow _) = do
  playSound (soundDir ++ "door.wav") False
  yellowKeyCount -= 1
  resetCurrentTile
checkCurTile gs (LockBlue _) = do
  playSound (soundDir ++ "door.wav") False
  blueKeyCount -= 1
  resetCurrentTile
checkCurTile gs (LockGreen _) = do
  playSound (soundDir ++ "door.wav") False
  resetCurrentTile
checkCurTile gs (LockRed _) = do
  playSound (soundDir ++ "door.wav") False
  redKeyCount -= 1
  resetCurrentTile
checkCurTile gs (GateFinal _) = do
  let lvl = _level gs
  return ()
  -- put $ LevelComplete lvl def
checkCurTile gs (Water _) = do
  when (not . _hasFlippers $ gs) die
checkCurTile gs (Sand _ _) = do
  gs <- get
  let playerIdx = currentIdx gs
  case gs ^. player.direction of
    Standing -> error "standing on sand?"
    DirLeft  -> moveSand (playerIdx - 1)
    DirRight -> moveSand (playerIdx + 1)
    DirUp    -> moveSand (playerIdx - boardW)
    DirDown  -> moveSand (playerIdx + boardW)

checkCurTile gs _ = return ()

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
