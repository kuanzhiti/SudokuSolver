*** SUDOKU SOLVER ***

- Project Structure
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

- Controls
**Key**                       **Action**
↑ ↓ ← →                   Navigate cursor around the 9×9 grid
1 – 9                     Place digit into the selected cell
Space / 0 / Backspace     Clear the selected cell
Enter                     Run constraint-propagation solver
Q / Esc                   Quit the application

- Getting Started 
1. Compile with GHC
- ghc --make Main.hs -o sudoku
2. Run the program
- ./sudoku