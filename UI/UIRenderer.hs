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
  putStr clearScreen
  putStrLn (boldGrid ++ "=== HASKELL TERMINAL SUDOKU ===" ++ resetStyle)
  putStrLn ""
  putStr (formatBoard (grid st) (cursor st))
  putStrLn ""
  putStrLn ("Status: " ++ status st)
  hFlush stdout

-- Render grid lines and highlight active cursor
formatBoard :: Grid Cell -> (Int, Int) -> String
formatBoard g (curR, curC) = unlines (concatMap renderRow [0..8])
  where
    renderRow r = 
      let lineStr = concat [ formatCell r c (g !! (r * 9 + c)) | c <- [0..8] ]
          rowOut  = insertBoxSeparators lineStr
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

    insertBoxSeparators rowStr = 
      -- Formats 3 columns per box group
      let chunks = [ take 9 rowStr, take 9 (drop 9 rowStr), drop 18 rowStr ]
      in head chunks ++ "|" ++ (chunks !! 1) ++ "|" ++ (chunks !! 2)