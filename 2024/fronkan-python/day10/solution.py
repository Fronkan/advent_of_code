from argparse import ArgumentParser
from pathlib import Path
from collections import deque


DIRECTIONS = [
    (-1,0), # up
    (0,1), # right
    (1,0), # down
    (0,-1), # left
]

def puzzle1(input_file: Path):
    grid = [[int(e) for e in line] for line in input_file.read_text().splitlines()]
    start_generator = ((row, col) for row, line in enumerate(grid) for col, val in enumerate(line) if val == 0)
    max_row = len(grid) - 1
    max_col = len(grid[0]) - 1

    total = 0
    for start in start_generator:
        stack = deque([start])
        tops = set()
        while stack:
            cur = stack.pop()
            cur_val = grid[cur[0]][cur[1]]
            for next in _next_steps(cur, max_row, max_col):
                next_val = grid[next[0]][next[1]]
                if next_val == cur_val+1:
                    if next_val == 9:
                        tops.add(next)
                    else:
                        stack.append(next)
        total += len(tops)
    return total


def _print(grid, visited):
    lines = []
    for ri, row in enumerate(grid):
        lines.append(
            "".join(str(c) if (ri,ci) in visited else "." for ci, c in enumerate(row))
        )
    print("\n".join(lines), end="\n\n")

def _next_steps(cur, max_row, max_col):
    for dr,dc in DIRECTIONS:
        next = (cur[0] + dr, cur[1]+dc)
        if 0 <= next[0] <= max_row and 0 <= next[1] <= max_col:
            yield next


def puzzle2(input_file: Path):
    grid = [[int(e) for e in line] for line in input_file.read_text().splitlines()]
    start_generator = ((row, col) for row, line in enumerate(grid) for col, val in enumerate(line) if val == 0)
    max_row = len(grid) - 1
    max_col = len(grid[0]) - 1

    total = 0
    for start in start_generator:
        stack = deque([start])
        trails = 0
        while stack:
            cur = stack.pop()
            cur_val = grid[cur[0]][cur[1]]
            for next in _next_steps(cur, max_row, max_col):
                next_val = grid[next[0]][next[1]]
                if next_val == cur_val+1:
                    if next_val == 9:
                        trails += 1
                    else:
                        stack.append(next)
        total += trails
    return total

if __name__ == "__main__":
    print("Day 10")

    parser = ArgumentParser()
    parser.add_argument("--example", action="store_true", default=False)
    args = parser.parse_args()

    if args.example:
        print("running example file")
        input_file = Path(__file__).parent / "example_input.txt"
    else:
        input_file = Path(__file__).parent / "input.txt"
    try:
        print("Puzzle 1:", puzzle1(input_file))
    except NotImplementedError:
        print("puzzle 1 not implemented yet")
    try:
        print("Puzzle 2:", puzzle2(input_file))
    except NotImplementedError:
        print("puzzle 2 not implemented yet")