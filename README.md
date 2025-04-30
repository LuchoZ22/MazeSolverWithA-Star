# Maze Generator and Solver in Haskell

## Overview

This project is a Haskell-based implementation of a **maze generator** using the **Depth-First Search (DFS)** algorithm and a **maze solver** using the **A\*** pathfinding algorithm. It aims to find the **shortest path** from a starting point to a destination within a maze, using an informed search strategy.

## Features

- **Maze Generation** using DFS with randomly shuffled neighbors.
- **Pathfinding** using the A\* algorithm with the **Manhattan distance** as a heuristic.
- **Priority Queue** implementation for managing nodes based on the evaluation function.
- **Interactive Menu** to select predefined mazes or generate a random one.
- **Text-based Visualization** of the maze and solution path.

## How It Works

- **Cells** in the maze have boolean values representing walls (up, right, down, left) and a visited flag.
- **A\*** uses three main functions:
  - `g(n)`: Cost from the start to the current node.
  - `h(n)`: Estimated cost from the current node to the goal (Manhattan distance).
  - `f(n) = g(n) + h(n)`: Total estimated cost.
- Nodes are explored in order of lowest `f(n)` value.

## File Structure

- `MazeGen.hs`: Maze generation logic and display functions.
- `NPQueue.hs`: Priority queue and node representation for A*.
- `AStar.hs`: Core A* algorithm implementation.
- `AStarMazeSolver.hs`: User interface and menu logic.

## Getting Started

To run the program:

1. Load the project in your Haskell environment.
2. Run the `menu` function located in `AStarMazeSolver.hs`.

```haskell
> :l AStarMazeSolver.hs
> menu
```

The menu allows you to:
- Solve predefined mazes.
- Generate a random maze of custom size.
- Select start and end points.
- View the solution visually in the terminal.




## Author

Luis Fernando Zapata Moya  
Email: [luiszapatam1@upb.edu](mailto:luiszapatam1@upb.edu)
