# Sudoku Solver

## Project Structure

<details>
<summary>View project structure</summary>

```text
.
├── Data/
│   ├── Sudoku.hs        # Board math, house lookup (rows, cols, boxes), and indices
│   └── Tri.hs           # 3-way branching tree data structure for 1D grid access
├── Helpers.hs           # Utility functions (list bundling, set helpers, uniqueness)
├── PencilSolver.hs      # Core solving logic (propagation + MRV backtracking)
├── Terminal.hs          # Low-level terminal setup, escape-code parser, and event loop
├── UIRenderer.hs        # ANSI rendering logic for board layout and cell highlights
├── UIState.hs           # State transitions and solver invocation
└── Main.hs              # Application entry point
```

</details>

## Controls

| Key | Action |
|---|---|
| `↑` `↓` `←` `→` | Navigate cursor around the 9×9 grid |
| `1` – `9` | Place a digit into the selected cell |
| `Space` / `0` / `Backspace` | Clear the selected cell |
| `Enter` | Run the constraint-propagation solver |
| `Q` / `Esc` | Quit the application |

## Getting Started

### 1. Compile with GHC

```bash
ghc --make Main.hs -o sudoku
```

### 2. Run the program

```bash
./sudoku
```