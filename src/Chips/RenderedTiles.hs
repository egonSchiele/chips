module Chips.RenderedTiles where
import Chips.Imports
import Chips.Types
import Chips.Globals

-- Given a tilemap (gotten with the `tileMap` function),
-- returns a list of all the tiles
renderedTiles :: [[Int]] -> [Tile]
renderedTiles tmap = renderTileMap tmap f (32, 32)
    where f 0  = Empty def -- 0 == where chip will be
          f 1  = Empty def
          f 2  = Wall def
          f 3  = Chip def
          f 4  = KeyYellow def
          f 5  = KeyRed def
          f 6  = KeyGreen def
          f 7  = KeyBlue def
          f 8  = LockYellow def
          f 9  = LockRed def
          f 10 = LockGreen def
          f 11 = LockBlue def
          f 12 = Gate def
          f 13 = GateFinal def
          f 14 = Help def
          f 15 = Amoeba def
          f 16 = Bee DirUp (Empty def) def
          f 17 = Bomb def
          f 18 = FFDown def
          f 19 = FFLeft def
          f 20 = FFRight def
          f 21 = FFUp def
          f 22 = FFRandom def
          f 23 = FFShoes def
          f 24 = FireBoots def
          f 25 = Fire def
          f 26 = Flippers def
          f 27 = Frog DirUp (Empty def) def
          f 28 = IceBottomLeft def
          f 29 = IceBottomRight def
          f 30 = IceSkates def
          f 31 = IceTopLeft def
          f 32 = IceTopRight def
          f 33 = Ice def
          f 34 = Sand (Empty def) def
          f 35 = Spy def
          f 36 = Tank DirUp (Empty def) def
          f 37 = WaterSplash def
          f 38 = Water def
          f 39 = Worm DirUp (Empty def) def
          f 40 = Sand (Chip def) def
          f 41 = Sand (Fire def) def
          f 42 = ButtonBlue def
          -- the locations of the traps get filled in later...
          f 43 = ButtonBrown (Arbitrary 0) def
          f 44 = ButtonRed def
          f 45 = ButtonGreen def
          f 46 = ToggleDoor True def
          f 47 = ToggleDoor False def
          f 48 = BallPink DirRight (Empty def) def
          f 49 = BallPink DirLeft (Empty def) def
          f 50 = BallPink DirUp (Empty def) def
          f 51 = BallPink DirDown (Empty def) def
          f 52 = Rocket DirUp (Empty def) def
          f 53 = Rocket DirDown (Empty def) def
          f 54 = Rocket DirLeft (Empty def) def
          f 55 = Rocket DirRight (Empty def) def
          f 56 = Fireball DirUp (Empty def) def
          f 57 = Fireball DirDown (Empty def) def
          f 58 = Fireball DirLeft (Empty def) def
          f 59 = Fireball DirRight (Empty def) def
          f 60 = GeneratorFireball DirUp def
          f 61 = GeneratorFireball DirDown def
          f 62 = GeneratorFireball DirLeft def
          f 63 = GeneratorFireball DirRight def
          f 64 = Trap (Empty def) def
          f 65 = InvisibleWall False def
          f 66 = InvisibleWall True def
          f 67 = BlueWall True def
          f 68 = BlueWall False def
          f 69 = Gravel def
          f 70 = FakeChip def
          f 71 = Teleporter (Arbitrary 50) (Arbitrary 50) (Arbitrary 50) (Arbitrary 50) def
          f 72 = RecessedWall def
          f 73 = ThinWall DirUp def
          f 74 = ThinWall DirDown def
          f 75 = ThinWall DirLeft def
          f 76 = ThinWall DirRight def
          f 77 = Dirt def
          f 78 = Sand (FFShoes def) def
          f 79 = Sand (Flippers def) def
          f 80 = Sand (FireBoots def) def
          f 81 = Sand (IceSkates def) def
