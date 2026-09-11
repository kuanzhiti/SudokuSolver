module Terminal
  ( withRawTerminal
  , readEvent
  , runLoop
  ) where

import System.IO
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)  -- <--- Added here
import Data.Char (isDigit, digitToInt)
import UI.UIState (UIState, Event(..), handleEvent)
import UI.UIRenderer (renderUI, hideCursor, showCursor)

-- | 1. Raw Mode Setup and Teardown
withRawTerminal :: IO a -> IO a
withRawTerminal action = bracket setup teardown (\_ -> action)
  where
    setup = do
      oldBuffering <- hGetBuffering stdin
      oldEcho      <- hGetEcho stdin

      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      putStr hideCursor
      hFlush stdout

      return (oldBuffering, oldEcho)

    teardown (oldBuffering, oldEcho) = do
      putStr showCursor
      hFlush stdout
      hSetBuffering stdin oldBuffering
      hSetEcho stdin oldEcho

-- | 2. Key Parser: ASCII & Multi-byte Escape Sequences
readEvent :: IO Event
readEvent = do
  c <- getChar
  case c of
    '\ESC' -> parseEscapeSequence

    'q'    -> return Quit
    'Q'    -> return Quit

    '\n'   -> return ConfirmSolve
    '\r'   -> return ConfirmSolve

    ' '    -> return ClearCell
    '\DEL' -> return ClearCell
    '\b'   -> return ClearCell
    '0'    -> return ClearCell

    d | isDigit d && d /= '0' -> return (SetDigit (digitToInt d))

    _      -> readEvent

-- | Replace the old parseEscapeSequence with this:
parseEscapeSequence :: IO Event
parseEscapeSequence = do
  -- Wait 20 milliseconds (20,000 microseconds) for subsequent bytes
  threadDelay 20000
  hasMore <- hReady stdin
  if not hasMore
    then return Quit
    else do
      c1 <- getChar
      case c1 of
        '[' -> do
          c2 <- getChar
          case c2 of
            'A' -> return MoveUp
            'B' -> return MoveDown
            'C' -> return MoveRight
            'D' -> return MoveLeft
            _   -> readEvent
        _   -> readEvent

-- | 3. Event Loop Dispatcher
runLoop :: UIState -> IO ()
runLoop st = do
  renderUI st
  ev <- readEvent
  case ev of
    Quit -> putStrLn "\nExiting Sudoku solver. Goodbye!"
    _    -> do
      let nextState = handleEvent ev st
      runLoop nextState