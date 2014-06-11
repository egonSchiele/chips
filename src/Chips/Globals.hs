{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

module Chips.Globals where
import Chips.Common

-- this keeps track of when we last moved.
-- So if the user is holding a key down, we
-- don't want to move too fast.
lastPress :: IORef UTCTime
lastPress = unsafePerformIO $ do
  now <- getCurrentTime
  newIORef now

-- This is a game clock that keeps
-- track of when to move a bee, a frog,
-- etc.
lastTick :: IORef UTCTime
lastTick = unsafePerformIO $ do
  now <- getCurrentTime
  newIORef now

-- Keeps track of where the user is right now
curLocation :: IORef Int
curLocation = unsafePerformIO $ newIORef 0

-- Keeps track of where the user was last move
prevLocation :: IORef Int
prevLocation = unsafePerformIO $ newIORef 0

-- if a user is holding a key down, move
-- this fast (currently every 1/4 of a second)
moveSpeed = -0.15

tileSize = 32

soundDir :: String
soundDir = "sounds/"

levelNames :: [String]
levelNames = [
  "LESSON 1",
  "LESSON 2",
  "LESSON 3",
  "LESSON 4",
  "LESSON 5",
  "LESSON 6",
  "LESSON 7",
  "LESSON 8",
  "NUTS AND BOLTS",
  "BRUSHFIRE",
  "TRINITY",
  "HUNT",
  "SOUTHPOLE",
  "TELEBLOCK",
  "ELEMENTARY",
  "CELLBLOCKED",
  "NICE DAY",
  "CASTLE MOAT",
  "DIGGER",
  "TOSSED SALAD"
  ]

passwords :: [String]
passwords = [
  "BDHP",
  "JXMJ",
  "ECBQ",
  "YMCJ",
  "TQKB",
  "WNLD",
  "FXQO",
  "NHAG",
  "KCRE",
  "UVWS",
  "CNPE",
  "WVHI",
  "OCKS"
  ]

boardW = 32 -- length . head $ tileMap
boardH = 32 -- length tileMap
