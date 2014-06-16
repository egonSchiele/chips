module Chips.Enemies where
import Chips.Types
import Chips.Utils
import Chips.Imports
import Chips.Globals
import Chips.Position
import Chips.Move

moveEnemies :: GameMonad ()
moveEnemies = do
  onTick $ do
    eachTile $ \(tile, i) -> do
      case tile of
        Tank dir _ _   -> maybeMoveTile i dir Nothing
        Rocket dir _ _ -> moveClockwiseLong i $ Just $ \(tile, moveI) ->
                            case tile of
                              Bomb _ -> do
                                moveTile i moveI Nothing
                                tileAt (Ix moveI) .= (Empty def)
                                return True
                              _ -> return False
        BallPink dir tileUnder _ -> maybeMoveTile i dir $ Just $ \_ -> do
                                      tileAt (Ix i) .= (BallPink (opposite dir) tileUnder def)
                                      return True
        Fireball dir _ _ -> moveClockwiseLong i $ Just $ \(tile, moveI) ->
                              case tile of
                                Fire _ -> moveTile i moveI (Just dir)
                                Water _ -> tileAt (Ix i) .= (Empty def) >> return True
                                Ice _ -> moveTile i moveI (Just dir)
                                _ -> return False
        Bee _ _ _ -> moveClockwise i Nothing
        Frog _ _ _ -> moveFrog i
        _       -> return False
      return ()

-- move this frog closer to the player
moveFrog :: Int -> GameMonad Bool
moveFrog i = do
  (_, time) <- use tick
  if odd time
    then return False
    else do
      frog <- use $ tileAt (Ix i)
      p <- use player
      let addDir dir = modify ((maybeMoveTile i dir Nothing):)
          -- list of all the moves the frog could make.
          -- We will make one of these moves.
          moves =
            with [] $ do
              when (p ^. x < frog ^. x) $ addDir DirLeft
              when (p ^. x > frog ^. x) $ addDir DirRight
              when (p ^. y < frog ^. y) $ addDir DirDown
              when (p ^. y > frog ^. y) $ addDir DirUp
      oneOf moves

-- Move this bee counter-clockwise around an object.
moveClockwise :: Int -> Maybe ((Tile, Int) -> GameMonad Bool) -> GameMonad Bool
moveClockwise i func = do
    enemy <- use $ tileAt (Ix i)
    let goLeft  = maybeMoveTile i DirLeft func
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
    enemy <- use $ tileAt (Ix i)
    let goLeft  = maybeMoveTile i DirLeft func
        goRight = maybeMoveTile i DirRight func
        goUp    = maybeMoveTile i DirUp func
        goDown  = maybeMoveTile i DirDown func
    case getDirection enemy of
      Just DirUp    -> goUp    <||> goLeft  <||> goRight <||> goDown
      Just DirLeft  -> goLeft  <||> goDown  <||> goUp    <||> goRight
      Just DirDown  -> goDown  <||> goRight  <||> goLeft  <||> goUp
      Just DirRight -> goRight <||> goUp    <||> goDown  <||> goLeft
      Nothing       -> error $ "don't know how to get direction for enemy: " ++ (show enemy)

