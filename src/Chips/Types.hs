{-# LANGUAGE TemplateHaskell #-}
module Chips.Types where
import Chips.Common
import Control.Lens
import ActionKid
import Graphics.Gloss hiding (display)

data Direction = DirUp | DirDown | DirLeft | DirRight | Standing deriving (Show, Eq)
data TilePos = Current | TileAbove | TileBelow | TileLeft | TileRight | Arbitrary Int | Coords (Int, Int) deriving (Show, Eq)

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
          | Amoeba Attributes
          | Bee { _beeDirection :: Direction, _tileUnderBee :: Tile, _beeAttrs :: Attributes }
          | Bomb Attributes
          | FFDown Attributes
          | FFLeft Attributes
          | FFRight Attributes
          | FFUp Attributes
          | FFRandom Attributes
          | FFShoes Attributes
          | FireBoots Attributes
          | Fire Attributes
          | Flippers Attributes
          | Frog { _frogDirection :: Direction, _tileUnderFrog :: Tile, _frogAttrs :: Attributes }
          | IceBottomLeft Attributes
          | IceBottomRight Attributes
          | IceSkates Attributes
          | IceTopLeft Attributes
          | IceTopRight Attributes
          | Ice Attributes
          | Sand { _tileUnderSand :: Tile, _sandAttrs :: Attributes }
          | Spy Attributes
          | Tank { _tankDirection :: Direction, _tileUnderTank :: Tile, _tankAttrs :: Attributes }
          | WaterSplash Attributes
          | Water Attributes
          | Worm { _wormDirection :: Direction, _tileUnderWorm :: Tile, _wormAttrs :: Attributes }
          | ButtonBlue Attributes
          | ButtonBrown { _trapLocation :: TilePos, _bbAttrs :: Attributes }
          | ButtonRed Attributes
          | ButtonGreen Attributes
          | ToggleDoor { _open :: Bool, _tdAttrs :: Attributes }
          | Trap { _inTrap :: Tile, _tdAttrs :: Attributes }
          | BallPink { _ballDirection :: Direction, _tileUnderBall :: Tile, _tdAttrs :: Attributes }
          | Rocket   { _rocketDirection :: Direction, _tileUnderRocket :: Tile, _tdAttrs :: Attributes }
          | Fireball { _fireballDirection :: Direction, _tileUnderFireball :: Tile, _tdAttrs :: Attributes }
          | GeneratorFireball { _generatorDir :: Direction, _gfAttrs :: Attributes }
          deriving (Show, Eq)

deriveMC ''Tile

instance Renderable Tile where
    render (Empty _)            = image "images/empty.png"
    render (Wall _)             = image "images/wall.png"
    render (Chip _)             = image "images/chip.png"
    render (KeyYellow _)        = image "images/key_yellow.png"
    render (KeyRed _)           = image "images/key_red.png"
    render (KeyGreen _)         = image "images/key_green.png"
    render (KeyBlue _)          = image "images/key_blue.png"
    render (LockYellow _)       = image "images/lock_yellow.png"
    render (LockRed _)          = image "images/lock_red.png"
    render (LockGreen _)        = image "images/lock_green.png"
    render (LockBlue _)         = image "images/lock_blue.png"
    render (Gate _)             = image "images/gate.png"
    render (GateFinal _)        = image "images/gate_final1.png"
    render (Help _)             = image "images/help.png"
    render (Amoeba _)           = image "images/amoeba.png"
    render (Bee DirUp _ _)        = image "images/bee_up.png"
    render (Bee DirDown _ _)      = image "images/bee_down.png"
    render (Bee DirLeft _ _)      = image "images/bee_left.png"
    render (Bee DirRight _ _)     = image "images/bee_right.png"
    render (Bomb _)             = image "images/bomb.png"
    render (FFDown _)           = image "images/ff_down.png"
    render (FFLeft _)           = image "images/ff_left.png"
    render (FFRight _)          = image "images/ff_right.png"
    render (FFUp _)             = image "images/ff_up.png"
    render (FFRandom _)         = image "images/ff_random.png"
    render (FFShoes _)          = image "images/ff_shoes.png"
    render (FireBoots _)        = image "images/fire_boots.png"
    render (Fire _)             = image "images/fire.png"
    render (Flippers _)         = image "images/flippers.png"
    render (Frog DirUp _ _)       = image "images/frog_up.png"
    render (Frog DirDown _ _)     = image "images/frog_down.png"
    render (Frog DirLeft _ _)     = image "images/frog_left.png"
    render (Frog DirRight _ _)    = image "images/frog_right.png"
    render (IceBottomLeft _)    = image "images/ice_bottom_left.png"
    render (IceBottomRight _)   = image "images/ice_bottom_right.png"
    render (IceSkates _)        = image "images/ice_skates.png"
    render (IceTopLeft _)       = image "images/ice_top_left.png"
    render (IceTopRight _)      = image "images/ice_top_right.png"
    render (Ice _)              = image "images/ice.png"
    render (Sand _ _)           = image "images/sand.png"
    render (Spy _)              = image "images/spy.png"
    render (Tank DirUp _ _)       = image "images/tank_up.png"
    render (Tank DirDown _ _)     = image "images/tank_down.png"
    render (Tank DirLeft _ _)     = image "images/tank_left.png"
    render (Tank DirRight _ _)    = image "images/tank_right.png"
    render (WaterSplash _)      = image "images/water_splash.png"
    render (Water _)            = image "images/water.png"
    render (Worm DirUp _ _)       = image "images/worm_up.png"
    render (Worm DirDown _ _)     = image "images/worm_down.png"
    render (Worm DirLeft _ _)     = image "images/worm_left.png"
    render (Worm DirRight _ _)    = image "images/worm_right.png"
    render (ButtonBlue _)       = image "images/button_blue.png"
    render (ButtonBrown _ _)      = image "images/button_brown.png"
    render (ButtonRed _)        = image "images/button_red.png"
    render (ButtonGreen _)      = image "images/button_green.png"
    render (ToggleDoor True _)  = image "images/toggle_door_open.png"
    render (ToggleDoor False _) = image "images/toggle_door_closed.png"
    render (Trap tile _)        =
      case tile of
        Empty _ -> image "images/trap.png"
        _ -> render tile
    render (BallPink _ _ _)       = image "images/ball_pink.png"
    render (Rocket DirUp _ _)     = image "images/rocket_up.png"
    render (Rocket DirDown _ _)   = image "images/rocket_down.png"
    render (Rocket DirLeft _ _)   = image "images/rocket_left.png"
    render (Rocket DirRight _ _)  = image "images/rocket_right.png"
    render (Fireball _ _ _)       = image "images/fireball.png"
    render (GeneratorFireball _ _) = image "images/generator_fireball.png"

data Player = Player {
                _direction :: Direction,
                _standingOn :: Tile,
                _ar :: Attributes
} deriving Show

makeLenses ''Player
deriveMC ''Player

instance Renderable Player where
    render p = case p ^. direction of
                 DirUp    ->
                   case p ^. standingOn of
                     Water _          -> image "images/player_swim_up.png"
                     Ice _            -> image "images/player_ice_up.png"
                     IceTopLeft _     -> image "images/player_ice_top_left_up.png"
                     IceTopRight _    -> image "images/player_ice_top_right_up.png"
                     IceBottomLeft _  -> image "images/player_ice_bottom_left_up.png"
                     IceBottomRight _ -> image "images/player_ice_bottom_right_up.png"
                     FFLeft _         -> image "images/player_ff_left_up.png"
                     FFRight _        -> image "images/player_ff_right_up.png"
                     FFUp _           -> image "images/player_ff_up_up.png"
                     FFDown _         -> image "images/player_ff_down_up.png"
                     Fire _           -> image "images/player_fire_up.png"
                     _                -> image "images/player_up.png"
                 DirDown  ->
                   case p ^. standingOn of
                     Water _          -> image "images/player_swim_down.png"
                     Ice _            -> image "images/player_ice_down.png"
                     IceTopLeft _     -> image "images/player_ice_top_left_down.png"
                     IceTopRight _    -> image "images/player_ice_top_right_down.png"
                     IceBottomLeft _  -> image "images/player_ice_bottom_left_down.png"
                     IceBottomRight _ -> image "images/player_ice_bottom_right_down.png"
                     FFLeft _         -> image "images/player_ff_left_down.png"
                     FFRight _        -> image "images/player_ff_right_down.png"
                     FFUp _           -> image "images/player_ff_up_down.png"
                     FFDown _         -> image "images/player_ff_down_down.png"
                     Fire _           -> image "images/player_fire_down.png"
                     _                -> image "images/player_down.png"
                 DirLeft  -> 
                   case p ^. standingOn of
                     Water _          -> image "images/player_swim_left.png"
                     Ice _            -> image "images/player_ice_left.png"
                     IceTopLeft _     -> image "images/player_ice_top_left_left.png"
                     IceTopRight _    -> image "images/player_ice_top_right_left.png"
                     IceBottomLeft _  -> image "images/player_ice_bottom_left_left.png"
                     IceBottomRight _ -> image "images/player_ice_bottom_right_left.png"
                     FFLeft _         -> image "images/player_ff_left_left.png"
                     FFRight _        -> image "images/player_ff_right_left.png"
                     FFUp _           -> image "images/player_ff_up_left.png"
                     FFDown _         -> image "images/player_ff_down_left.png"
                     Fire _           -> image "images/player_fire_left.png"
                     _                -> image "images/player_left.png"
                 DirRight ->
                   case p ^. standingOn of
                     Water _          -> image "images/player_swim_right.png"
                     Ice _            -> image "images/player_ice_right.png"
                     IceTopLeft _     -> image "images/player_ice_top_left_right.png"
                     IceTopRight _    -> image "images/player_ice_top_right_right.png"
                     IceBottomLeft _  -> image "images/player_ice_bottom_left_right.png"
                     IceBottomRight _ -> image "images/player_ice_bottom_right_right.png"
                     FFLeft _         -> image "images/player_ff_left_right.png"
                     FFRight _        -> image "images/player_ff_right_right.png"
                     FFUp _           -> image "images/player_ff_up_right.png"
                     FFDown _         -> image "images/player_ff_down_right.png"
                     Fire _           -> image "images/player_fire_right.png"
                     _                -> image "images/player_right.png"
                 Standing ->
                   case p ^. standingOn of
                     Water _          -> image "images/player_swim_down.png"
                     Ice _            -> image "images/player_ice_down.png"
                     IceTopLeft _     -> image "images/player_ice_top_left_down.png"
                     IceTopRight _    -> image "images/player_ice_top_right_down.png"
                     IceBottomLeft _  -> image "images/player_ice_bottom_left_down.png"
                     IceBottomRight _ -> image "images/player_ice_bottom_right_down.png"
                     FFLeft _         -> image "images/player_ff_left_down.png"
                     FFRight _        -> image "images/player_ff_right_down.png"
                     FFUp _           -> image "images/player_ff_up_down.png"
                     FFDown _         -> image "images/player_ff_down_down.png"
                     Fire _           -> image "images/player_fire_down.png"
                     _                -> image "images/player_down.png"

data GameState = GameState {
                    _tiles :: [Tile],
                    _player :: Player,
                    _level :: Int,
                    _redKeyCount :: Int,
                    _blueKeyCount :: Int,
                    _yellowKeyCount :: Int,
                    _hasGreenKey :: Bool,
                    _hasFFShoes :: Bool,
                    _hasFireBoots :: Bool,
                    _hasFlippers :: Bool,
                    _hasIceSkates :: Bool,
                    _godMode :: Bool,
                    _disableInput :: Bool,
                    _ga :: Attributes
               }

makeLenses ''GameState
deriveMC ''GameState

instance Renderable GameState where
    -- render (LevelComplete _ _) = translate 0 200 $ scale 0.1 0.1 $ text "Level Complete! Press space to go onward!"
    render gs = displayAll (_tiles gs) <> display (_player gs)

type GameMonad a = StateT GameState IO a

type LevelNumber = Int
