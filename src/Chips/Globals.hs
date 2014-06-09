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
