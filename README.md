# Advent of Code OCaml Solutions

This repository contains solutions for [Advent of Code](https://adventofcode.com/) puzzles implemented in OCaml.\
Author: Nejc Zajc\
Year: 2025


## Prerequisites

To run these solutions, you'll need:
- OCaml (recommended version 4.14.0 or higher)
- Dune build system
- Make

Ocaml packages used (in specific solutions, fresh project doesn't need these)
- digestif `opam install digestif`
- yojson `opam install yojson`

## Project Structure

```
.
├── data/              # Input data files
│   ├── 2015/          # Input files for 2015 puzzles
│   └── 2024/          # Input files for 2024 puzzles
│   └── 2025/          # Input files for 2025 puzzles
├── out/               # Solutions of the problems
│   ├── 2015/          # Output files for 2015 puzzles
│   └── 2024/          # Output files for 2024 puzzles
│   └── 2025/          # Output files for 2025 puzzles
├── src/               
│   ├── main/          # Main application entry point
│   ├── solvers/       # Puzzle solutions by year
│   └── utils/         # Utility functions
```

## How to Run

To build the project (after any code changes), run the following command:

```bash
dune build
```

To run a specific puzzle solution, use the following command:

```bash
make run ARGS="YEAR DAY PART"
```

Where:
- `YEAR`: The year of the puzzle (e.g. 2024)
- `DAY`: The day number (1-25)
- `PART`: The part number (1 or 2)

For example, to run Day 1, Part 1 from 2024:

```bash
make run ARGS="2024 1 1"
```

## Adding New Solutions

1. Create a new file in `src/solvers/YEAR/dayXX.ml`
2. Place your input data in `data/YEAR/day_XX.in`
3. Implement the solution following the solver signature
4. Update the `src/main/main.ml` with the solver for that day

## License

This project is open source and available under the MIT License.
