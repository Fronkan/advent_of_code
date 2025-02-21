from argparse import ArgumentParser
from collections import deque
from pathlib import Path

def puzzle1(input_file: Path):
    grid = [
        [char for char in line]
        for line in input_file.read_text().splitlines()
    ]
    max_row = len(grid)
    max_col = len(grid[0])
    visited: set[tuple[int, int]] = set()
    total = 0
    for ri in range(max_row):
        for ci in range(max_col):
            if (ri, ci) in visited:
                continue
            total += calculate_region_cost(ri,ci, grid, visited)
    return total


def calculate_region_cost(root_ri:int, root_ci:int, grid: list[list[str]], visited: set[tuple[int, int]]):
    plant = grid[root_ri][root_ci]
    region = {(root_ri, root_ci)}
    to_expand: deque[tuple[int, int]] = deque()
    to_expand.append((root_ri, root_ci))
    perimeter: deque[tuple[int, int]] = deque()
    while to_expand:
        ri, ci = to_expand.pop()
        for dr in [-1,0,1]:
            for dc in [-1, 0, 1]:
                if abs(dr) == abs(dc):
                    continue
                point = (ri+dr, ci+dc)
                if not is_in_grid(grid, *point):
                    perimeter.append(point)
                elif grid[point[0]][point[1]] == plant:
                    if point not in region:
                        region.add(point)
                        to_expand.append(point)
                else:
                    perimeter.append(point)
    for point in region:
        visited.add(point)

    return len(region)*len(perimeter)

def is_in_grid(grid, ri, ci):
    return all([
        0<=ri<len(grid),
        0<=ci<len(grid[0]),
    ])

def puzzle2(input_file: Path):
    grid = [
        [char for char in line]
        for line in input_file.read_text().splitlines()
    ]
    max_row = len(grid)
    max_col = len(grid[0])
    visited: set[tuple[int, int]] = set()
    total = 0
    for ri in range(max_row):
        for ci in range(max_col):
            if (ri, ci) in visited:
                continue
            total += calculate_region_cost2(ri,ci, grid, visited)
    return total


def calculate_region_cost2(root_ri:int, root_ci:int, grid: list[list[str]], visited: set[tuple[int, int]]):
    plant = grid[root_ri][root_ci]
    region = {(root_ri, root_ci)}
    to_expand: deque[tuple[int, int]] = deque()
    to_expand.append((root_ri, root_ci))
    perimeter: deque[tuple[int, int, int, int]] = deque()
    while to_expand:
        ri, ci = to_expand.pop()
        for dr in [-1,0,1]:
            for dc in [-1, 0, 1]:
                if abs(dr) == abs(dc):
                    continue
                point = (ri+dr, ci+dc)
                if not is_in_grid(grid, *point):
                    perimeter.append((*point, dr, dc))
                elif grid[point[0]][point[1]] == plant:
                    if point not in region:
                        region.add(point)
                        to_expand.append(point)
                else:
                    perimeter.append((*point, dr, dc))
    for point in region:
        visited.add(point)

    sides = []
    perimeter = deque(sorted(perimeter))
    while perimeter:
        point = perimeter.pop()
        found = False
        for side in sides:
            for point2 in side:
                if all([
                    (abs(point[0] - point2[0]) + abs(point[1] - point2[1])) == 1,
                    point[2] == point2[2],
                    point[3] == point2[3],
                ]):
                    side.append(point)
                    found = True
                    break
        if not found:
            sides.append([point])
    return len(region)*len(sides)


if __name__ == "__main__":
    print("Day 12")

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