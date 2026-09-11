module Main where

import System.IO
import Terminal (withRawTerminal, runLoop)
import UI.UIState (UIState(..))
import Data.Sudoku (emptyGrid)

initialState :: UIState
initialState = UIState
  { grid   = emptyGrid
  , cursor = (0, 0)
  , status = "Use arrow keys to move, 1-9 to input, Space to clear, Enter to solve, Q to quit."
  }

main :: IO ()
main = do
  -- Ensure stdout flushes immediately when rendering
  hSetBuffering stdout NoBuffering
  withRawTerminal (runLoop initialState)