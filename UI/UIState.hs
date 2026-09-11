module UIState where

import Data.Sudoku (Cell, Digit, Grid, emptyGrid)
import PencilSolver (solve) -- Member 1's Pure Solver

data Event 
  = MoveUp | MoveDown | MoveLeft | MoveRight
  | SetDigit Digit | ClearCell | ConfirmSolve | Quit
  deriving (Eq, Show)

data UIState = UIState
  { grid    :: Grid Cell
  , cursor  :: (Int, Int)
  , status  :: String
  } deriving (Show, Eq)

handleEvent :: Event -> UIState -> UIState
handleEvent event st@(UIState g (r, c) msg) = case event of
  -- Movement and Editing
  MoveUp     -> st { cursor = (max 0 (r - 1), c) }
  MoveDown   -> st { cursor = (min 8 (r + 1), c) }
  MoveLeft   -> st { cursor = (r, max 0 (c - 1)) }
  MoveRight  -> st { cursor = (r, min 8 (c + 1)) }
  
  SetDigit d -> 
    let idx = r * 9 + c
        g'  = take idx g ++ [Just d] ++ drop (idx + 1) g
    in st { grid = g', status = "Digit set." }
    
  ClearCell  -> 
    let idx = r * 9 + c
        g'  = take idx g ++ [Nothing] ++ drop (idx + 1) g
    in st { grid = g', status = "Cell cleared." }

  -- Solving Phase Trigger
  ConfirmSolve ->
    case solve g of
      Just solvedGrid -> 
        st { grid   = map Just solvedGrid
           , status = "SUCCESS: Puzzle solved!" 
           }
      Nothing -> 
        st { status = "ERROR: Invalid or unsolvable Sudoku board!" }

  Quit -> st