{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import Chips

main = do
    print chipStart
    playSound (soundDir ++ "chips01.wav") True
    run "chips challenge" (9 * tileSize, 9 * tileSize) gameState on stepGame

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
