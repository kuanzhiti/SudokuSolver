@echo off
echo Building Sudoku Solver...
ghc --make Main.hs -o sudoku.exe

if %ERRORLEVEL% EQU 0 (
    echo Build successful! Starting game...
    sudoku.exe
) else (
    echo Build failed.
)