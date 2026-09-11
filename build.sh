#!/usr/bin/env bash

echo "Building Sudoku Solver..."
ghc --make Main.hs -o sudoku

if [ $? -eq 0 ]; then
    echo "Build successful! Starting game..."
    ./sudoku
else
    echo "Build failed."
fi