{-# LANGUAGE TemplateHaskell #-}
module Chips.Types where
import Chips.Common
import Control.Lens
import ActionKid
import Graphics.Gloss hiding (display)

data Tile = Empty Attributes
          | Wall  Attributes
          | Chip  Attributes
          | KeyYellow Attributes
          | KeyRed Attributes
          | KeyGreen Attributes
          | KeyBlue Attributes
          | LockYellow Attributes
          | LockRed Attributes
          | LockGreen Attributes
          | LockBlue Attributes
          | Gate Attributes
          | GateFinal Attributes
          | Help Attributes
          deriving (Show, Eq)

deriveMC ''Tile

instance Renderable Tile where
    render (Empty _)      = image "images/empty.png"
    render (Wall _)       = image "images/wall.png"
    render (Chip _)       = image "images/chip.png"
    render (KeyYellow _)  = image "images/key_yellow.png"
    render (KeyRed _)     = image "images/key_red.png"
    render (KeyGreen _)   = image "images/key_green.png"
    render (KeyBlue _)    = image "images/key_blue.png"
    render (LockYellow _) = image "images/lock_yellow.png"
    render (LockRed _)    = image "images/lock_red.png"
    render (LockGreen _)  = image "images/lock_green.png"
    render (LockBlue _)   = image "images/lock_blue.png"
    render (Gate _)       = image "images/gate.png"
    render (GateFinal _)  = image "images/gate_final1.png"
    render (Help _)       = image "images/help.png"

data Direction = DirUp | DirDown | DirLeft | DirRight | Standing

data Player = Player {
                _direction :: Direction,
                _ar :: Attributes
}

makeLenses ''Player
deriveMC ''Player

instance Renderable Player where
    render p = case p ^. direction of
                 DirUp    -> image "images/player_up.png"
                 DirDown  -> image "images/player_down.png"
                 DirLeft  -> image "images/player_left.png"
                 DirRight -> image "images/player_right.png"
                 Standing -> image "images/player_down.png"

data GameState = GameState {
                    _tiles :: [Tile],
                    _player :: Player,
                    _level :: Int,
                    _levelName :: String,
                    _password :: String,
                    _redKeyCount :: Int,
                    _blueKeyCount :: Int,
                    _yellowKeyCount :: Int,
                    _hasGreenKey :: Bool,
                    _ga :: Attributes
               } | LevelComplete {
                    _levelNum :: Int,
                    _lca :: Attributes
               }

makeLenses ''GameState
deriveMC ''GameState

instance Renderable GameState where
    render (LevelComplete _ _) = translate 0 200 $ scale 0.1 0.1 $ text "Level Complete! Press space to go onward!"
    render gs = displayAll (_tiles gs) <> display (_player gs)
