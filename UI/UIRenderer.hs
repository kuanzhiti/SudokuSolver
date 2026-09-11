module UI.UIRenderer where

import System.IO
import Data.Sudoku (Cell, Digit, Grid)
import UI.UIState (UIState(..))

-- ANSI Escape Helper Codes
clearScreen :: String
clearScreen = "\ESC[2J\ESC[H"

hideCursor :: String
hideCursor = "\ESC[?25l"

showCursor :: String
showCursor = "\ESC[?25h"

bgHighlight :: String
bgHighlight = "\ESC[47m\ESC[30m" -- Inverse colors for active cell

resetStyle :: String
resetStyle = "\ESC[0m"

boldGrid :: String
boldGrid = "\ESC[1m"

-- Render top-level UI
renderUI :: UIState -> IO ()
renderUI st = do
  putStr hideCursor
  putStr clearScreen
  putStrLn (boldGrid ++ "=== HASKELL TERMINAL SUDOKU ===" ++ resetStyle)
  putStrLn ""
  putStr (formatBoard (grid st) (cursor st))
  putStrLn ""
  putStrLn ("Status: " ++ status st ++ resetStyle)
  hFlush stdout

-- Render grid lines and highlight active cursor
formatBoard :: Grid Cell -> (Int, Int) -> String
formatBoard g (curR, curC) = unlines (concatMap renderRow [0..8])
  where
    renderRow r = 
      let -- Generate a list of 9 formatted cell strings
          cellsInRow = [ formatCell r c (g !! (r * 9 + c)) | c <- [0..8] ]
          
          -- Safely chunk into 3 groups of 3 cells each
          box1 = concat (take 3 cellsInRow)
          box2 = concat (take 3 (drop 3 cellsInRow))
          box3 = concat (drop 6 cellsInRow)
          
          rowOut = box1 ++ "|" ++ box2 ++ "|" ++ box3
      in if r > 0 && r `mod` 3 == 0
           then ["------+-------+------", rowOut]
           else [rowOut]

    formatCell r c cellVal =
      let digitChar = case cellVal of
            Nothing -> "."
            Just d  -> show d
          isCursor = (r == curR && c == curC)
      in if isCursor
           then bgHighlight ++ " " ++ digitChar ++ " " ++ resetStyle
           else " " ++ digitChar ++ " "