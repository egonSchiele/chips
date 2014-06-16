module Chips.GameState where
import Chips.Types
import Chips.Utils
import Chips.Imports
import Chips.Globals
import qualified Data.ByteString.Lazy as B
import Chips.RenderedTiles

-- given a number, returns the 2-d array that represents the tilemap for
-- that level.
tileMap :: Int -> IO [[Int]]
tileMap i = do
    contents <- B.readFile $ "maps/" ++ (show i) ++ ".json"
    let decoded = eitherDecode contents :: Either String [[Int]]
    case decoded of
      Left err -> error err
      Right tmap -> return tmap

-- tell all the brown buttons about the traps they are responsible for.
wireTraps :: Int -> [Tile] -> [Tile]
wireTraps i tmap =
    case lookup i trapButtons of
      Nothing -> tmap
      (Just list) -> foldl func tmap list
        where func tmap_ (i,j) = setValue tmap_ i $ \val ->
                 case val of
                   ButtonBrown _ attrs_ -> case (tmap !! j) of
                     Trap _ _ -> ButtonBrown (Ix j) attrs_
                     x -> error $ "item at location j: " ++ (show j) ++ " is not a Trap. It is a " ++ (show x)
                   x -> error $ "item at location i: " ++ (show i) ++ " is not a ButtonBrown. It is a " ++ (show x)

-- tell all the teleporters their teleport destinations
wireTeleporters :: Int -> [Tile] -> [Tile]
wireTeleporters i tmap =
    case lookup i teleportDestinations of
      Nothing -> tmap
      (Just list) -> foldl func tmap list
        where func tmap_ (i,u,d,l,r) = setValue tmap_ i $ \val ->
                 case val of
                   Teleporter _ _ _ _ attrs_ -> Teleporter (Ix u) (Ix d) (Ix l) (Ix r) attrs_
                   x -> error $ "item at location i: " ++ (show i) ++ " is not a Teleporter It is a " ++ (show x)

-- Given a level number, returns the starting game state for that level
gameState :: Int -> GameState
gameState i = x .~ startX $ y .~ startY $ gs
  where player_ = (Player Standing (Empty def) def)
        tmap = unsafePerformIO $ tileMap i
        finalMap = (wireTeleporters i) . (wireTraps i) . renderedTiles $ tmap
        gs = GameState
              finalMap
              (x .~ ((fst chipStart)*tileSize) $ y .~ ((snd chipStart)*tileSize) $ player_)
              i
              0 0 0 False
              False False False False False
              False
              (False, 0)
              def
        startX = if fst chipStart >= 4
                   then (4 - fst chipStart) * tileSize -- "4" is (9 - 1)/2. 9 is the width of the game screen
                   else 0

        startY = if snd chipStart <= (boardH-4)
                   then (4 - snd chipStart) * tileSize
                   else (9-boardH) * tileSize
        -- | (x, y) of chip's start position (marked as a 0 on the tile map)
        chipStart :: (Float, Float)
        chipStart =
          case findIndex (\xs -> 0 `elem` xs) tmap of
            Nothing -> error "You need to mark where chip will stand in the tilemap. Mark it with a zero (0)."
            Just y -> (fromIntegral . fromJust $ findIndex (==0) (tmap !! y), boardH - 1 - (fromIntegral y))

