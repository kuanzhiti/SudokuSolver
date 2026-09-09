module PencilSolver where

import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Sudoku
import Data.Tri
import Helpers

data Candidate = Placed !Digit | Pencil !Row !Col !(Set Digit)
  deriving (Show, Eq)

isPlaced :: Candidate -> Bool
isPlaced (Placed _) = True
isPlaced _          = False

type Worklist = [Int]

fullGrid :: Tri Candidate
fullGrid =
  fromList $
    map (\(r, c) -> Pencil r c (Set.fromList digits)) allRowsAndCols

pencilmark :: Grid Cell -> (Tri Candidate, Worklist)
pencilmark cellGrid = (finalGrid, emptyIndices)
  where
    indexedCells = zip3 allRowsAndCols [0..] cellGrid
    givens       = [ (r, c, d) | ((r, c), _, Just d) <- indexedCells ]
    emptyIndices = [ idx | (_, idx, Nothing) <- indexedCells ]
    finalGrid    = placeAll fullGrid givens

place :: Row -> Col -> Digit -> Tri Candidate -> (Tri Candidate, Worklist)
place r c d tri = (finalTree, affectedIndices)
  where
    targetIdx = toIndex r c
    triWithPlaced = update targetIdx (Placed d) tri

    (finalTree, affectedIndices) = foldl removeCandidate (triWithPlaced, []) (sees r c)

    removeCandidate (currentTree, accWorklist) idx =
      case currentTree ! idx of
        Pencil row col candidateSet ->
          if Set.member d candidateSet
            then
              let updatedSet  = Set.delete d candidateSet
                  updatedTree = update idx (Pencil row col updatedSet) currentTree
              in (updatedTree, idx : accWorklist)
            else (currentTree, accWorklist)
        Placed _ -> (currentTree, accWorklist)

placeAll :: Tri Candidate -> [(Row, Col, Digit)] -> Tri Candidate
placeAll tri rcds = foldl (\g (r, c, d) -> fst (place r c d g)) tri rcds

-- Propagates constraints, then falls back to backtracking search if necessary
solve :: Grid Cell -> Maybe (Grid Digit)
solve cellGrid = 
  let (initialGrid, initialWorklist) = pencilmark cellGrid
  in propagateAndSolve initialGrid initialWorklist

propagateAndSolve :: Tri Candidate -> Worklist -> Maybe (Grid Digit)
propagateAndSolve grid [] = solveWithBacktracking grid
propagateAndSolve grid (idx : rest) =
  case grid ! idx of
    Placed _ -> propagateAndSolve grid rest
    Pencil r c candidates ->
      case Set.toList candidates of
        []  -> Nothing -- Invalid state (Dead end)
        [singleDigit] ->
          let (grid', newWork) = place r c singleDigit grid
          in propagateAndSolve grid' (newWork ++ rest)
        _   -> propagateAndSolve grid rest

solveWithBacktracking :: Tri Candidate -> Maybe (Grid Digit)
solveWithBacktracking grid
  | all isPlaced candidatesList = Just [ d | Placed d <- candidatesList ]
  | otherwise =
      -- MRV Heuristic: Find pencil cell with the fewest remaining candidates
      case findMinCandidates grid of
        Nothing -> Nothing
        Just (idx, Pencil r c candidates) ->
          let tryDigit d =
                let (grid', newWork) = place r c d grid
                in propagateAndSolve grid' newWork
          in mhead $ mapMaybe tryDigit (Set.toList candidates)
        _ -> Nothing
  where
    candidatesList = [ grid ! i | i <- [0..80] ]

findMinCandidates :: Tri Candidate -> Maybe (Int, Candidate)
findMinCandidates grid =
  let unplaced = [ (i, grid ! i) | i <- [0..80], not (isPlaced (grid ! i)) ]
      candidateCount (_, Pencil _ _ s) = Set.size s
      candidateCount _                 = 999
  in case unplaced of
       [] -> Nothing
       xs -> Just $ minimumBy (\a b -> compare (candidateCount a) (candidateCount b)) xs