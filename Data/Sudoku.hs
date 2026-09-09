module Data.Sudoku where

import Data.List (delete, transpose)
import Helpers

newtype Row = Row Int deriving (Eq, Show)
newtype Col = Col Int deriving (Eq, Show)
newtype Box = Box Int deriving (Eq, Show)

type Digit = Int
type Cell = Maybe Digit
type Grid a = [a]

gridSize, boxSize, boxGridSize :: Int
gridSize = 9
boxSize = 3
boxGridSize = gridSize `div` boxSize

digits :: [Digit]
digits = [1..9]

isDigit :: Int -> Bool
isDigit n = 1 <= n && n <= 9

emptyGrid :: Grid Cell
emptyGrid = replicate (gridSize * gridSize) Nothing-- TODO: part 1

-- For the Brute-force Solver
rows :: Grid a -> [[a]]
rows = bundle gridSize-- TODO: part 1

cols :: Grid a -> [[a]]
cols = transpose . bundle gridSize-- TODO: part 1

boxes :: Grid a -> [[a]]
boxes grid = 
  box' fstrows ++ box' sndrows ++ box' trdrows
  where
    rs = rows grid --[[a]]
    fstrows = take boxSize rs
    sndrows = take boxSize (drop boxSize rs)
    trdrows = drop (2 * boxSize) rs
    box' xs = map concat tpbundles
      where
        rowbundles = map (bundle boxSize) xs
        tpbundles = transpose rowbundles
        

    
-- TODO: part 1
-- For the Pencilmarking Solver
{-|
Given a row and column, finds the (unique) list of all indices that
the cell at that position can see (i.e. all the cells that share the same
houses as it).
-}
sees :: Row -> Col -> [Int]
sees r c = rowcol ++ uqboxnos
  where
    boxno = boxOf r c
    rownos = rowIndices r
    colnos = colIndices c
    boxnos = boxIndices boxno
    self = toIndex r c
    rowcol = [x| x <- rownos ++ colnos, x /= self]
    uqboxnos = [x| x <- boxnos, x `notElem` rowcol && x /= self]

 -- TODO: part 1

{-|
Returns a list of all coordinates of the grid, in left-to-right,
top-to-bottom order.
-}
allRowsAndCols :: [(Row, Col)]
allRowsAndCols = [(Row r, Col c) | r <- [0..gridSize-1], c <- [0..gridSize-1]]

{-|
Converts a row and a column from the 2D grid into
a 1D index.
-}
toIndex :: Row -> Col -> Int
toIndex (Row i) (Col j) = i * gridSize + j

{-|
Finds the numbered box that a given cell occupies.
-}
boxOf :: Row -> Col -> Box
boxOf (Row r) (Col c) =
  Box ((r `div` boxSize) * boxGridSize + (c `div` boxSize))

{-|
Returns the indices of all cells in a given row.
-}
rowIndices :: Row -> [Int]
rowIndices (Row i) = [i*gridSize..i*gridSize+gridSize-1]

{-|
Returns the indices of all cells in a given column.
-}
colIndices :: Col -> [Int]
colIndices (Col j) = [j,j+gridSize..j+gridSize*(gridSize-1)]

{-|
Returns the indices of all cells in a given box.
-}
boxIndices :: Box -> [Int]
boxIndices (Box b) = [topLeft + i*gridSize + j | i <- [0..boxSize-1]
                                               , j <- [0..boxSize-1]]
  where (br, bc) = b `divMod` boxGridSize
        topLeft = br*gridSize*boxSize + bc*boxSize
