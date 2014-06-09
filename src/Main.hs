{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import Chips
main = do
    -- playSound (soundDir ++ "chips01.wav") True
                                                                    -- move this logic to ActionKid instead
    run "chips challenge" (9 * tileSize, 9 * tileSize) (gameState 1) on stepGame

stepGame :: Float -> GameMonad ()
stepGame i = do
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
  curI <- tilePosToIndex Current
  curLocation_ <- liftIO $ readIORef curLocation
  liftIO $ prevLocation $= curLocation_
  liftIO $ curLocation $= curI
  player.standingOn .= curTile
  case curTile of
    ButtonRed   _ -> once $ checkCurTile curTile
    ButtonBlue  _ -> once $ checkCurTile curTile
    ButtonGreen _ -> once $ checkCurTile curTile
    _ -> checkCurTile curTile

  cur <- liftIO getCurrentTime
  last <- liftIO $ readIORef lastTick
  when (diffUTCTime last cur < moveSpeed) $ do
    liftIO $ lastTick $= cur
    moveEnemies

maybeDisableInput = do
  curTile <- tilePosToTile Current
  gs <- get
  when (not $ gs ^. godMode) $ do
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
