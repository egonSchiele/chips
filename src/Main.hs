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
        when (gs ^. player.x > (4*tileSize) && gs ^. player.x < ((boardW-4)*tileSize)) $ do
          x += tileSize
    DirRight -> do
      maybeMove TileRight $ do
        player.x += tileSize
        when (gs ^. player.x > (3*tileSize) && gs ^. player.x < ((boardW-5)*tileSize)) $ do
          x -= tileSize
    DirUp    -> do
      maybeMove TileAbove $ do
        player.y += tileSize
        when (gs ^. player.y < ((boardH-5)*tileSize) && gs ^. player.y > (3*tileSize)) $ do
          y -= tileSize
    DirDown  -> do
      maybeMove TileBelow $ do
        player.y -= tileSize
        when (gs ^. player.y < ((boardH-4)*tileSize) && gs ^. player.y > (4*tileSize)) $ do
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
  if (diffUTCTime last cur < moveSpeed)
    then do
      liftIO $ lastTick $= cur
      tick .= True
    else
      tick .= False
  moveEnemies
  checkSand

-- this function checks if there
-- are any sand blocks on brown buttons.
-- If so, the related traps should be open.
checkSand = do
  gs <- get
  forM_ (withIndices (gs ^. tiles)) $ \(tile, i) -> do
    case tile of
      Sand button@(ButtonBrown _ _) _ -> checkCurTile button
      _ -> return ()

maybeDisableInput = do
  curTile <- tilePosToTile Current
  gs <- get
  if (not $ gs ^. godMode)
    then do
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
        -- trap disables/enables input by itself, dont mess with it
        Trap _ _         -> return ()
        _                -> do
          when (gs ^. disableInput) $ do
            player.direction .= Standing
            disableInput .= False
            liftIO . putStrLn $ "enabling input"
    else disableInput .= False
