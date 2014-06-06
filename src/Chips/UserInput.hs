module Chips.UserInput where
import Chips.Types
import Chips.Utils
import Chips.Common
import Chips.Core

-- this keeps track of when we last moved.
-- So if the user is holding a key down, we
-- don't want to move too fast.
lastPress :: IORef UTCTime
lastPress = unsafePerformIO $ do
  now <- getCurrentTime
  newIORef now

-- if a user is holding a key down, move
-- this fast (currently every 1/4 of a second)
moveSpeed = -0.25

-- used in some logic that lets a user hold a key down.
resetMoveTime :: IO ()
resetMoveTime = modifyIORef lastPress (addUTCTime moveSpeed)

on (EventKey (SpecialKey KeyLeft) Down _ _) gs = do
    resetMoveTime
    return $ player.direction .~ DirLeft $ gs

on (EventKey (SpecialKey KeyRight) Down _ _) gs = do
    resetMoveTime
    return $ player.direction .~ DirRight $ gs

on (EventKey (SpecialKey KeyUp) Down _ _) gs = do
    resetMoveTime
    return $ player.direction .~ DirUp $ gs

on (EventKey (SpecialKey KeyDown) Down _ _) gs = do
    resetMoveTime
    return $ player.direction .~ DirDown $ gs

-- on (EventKey (SpecialKey KeySpace) Down _ _) gs = return gameState
on _ gs = return $ player.direction .~ Standing $ gs

maybeMove :: (GameState -> Tile) -> GameState -> GameState -> IO GameState
maybeMove func gs newGs = do
    cur <- getCurrentTime
    last <- readIORef lastPress
    -- if we are holding a key down, we would move very fast.
    -- but in the game, there is a bit of a delay, chip doesn't ZOOM
    -- across the screen. This code slows chip down...so if the last 
    -- time we moved was too recently, don't move. Just return the
    -- same gameState.
    --
    -- This "lastPress" time gets reset every time you press a key,
    -- so if you keep pumping a direction key, you can move as fast
    -- as you can keep jamming on the key. But if you hold a key down,
    -- you will move as fast as `moveSpeed`.
    if diffUTCTime last cur > moveSpeed
      then return gs
      else do
        lastPress $= cur
        case func gs of
          Wall _ -> do
            oof
            return gs
          LockRed _    -> if _redKeyCount gs > 0
                            then return newGs
                            else oof >> return gs
          LockBlue _   -> if _blueKeyCount gs > 0
                            then return newGs
                            else oof >> return gs
          LockGreen _  -> if _hasGreenKey gs
                            then return newGs
                            else oof >> return gs
          LockYellow _ -> if _yellowKeyCount gs > 0
                            then return newGs
                            else oof >> return gs
          Gate _       -> if chipsLeft gs == 0
                            then return newGs
                            else oof >> return gs
          _ -> return newGs
