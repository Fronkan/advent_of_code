from argparse import ArgumentParser
from pathlib import Path

def puzzle1(input_file: Path):
    obstacles = set()
    guard_pos = None
    lines = input_file.read_text().splitlines()
    for y, line in zip(range(len(lines)-1, -1, -1), lines):
        for x, char in enumerate(line):
            if char == "#":
                obstacles.add(
                    (x,y) 
                )
            if char == "^":
                guard_pos = (x,y)

    max_y, max_x = len(lines)-1, len(lines[0])-1

    assert guard_pos is not None

    print(max_y, max_x, guard_pos)
    direction = (0,1)
    visited = set([guard_pos])
    while _is_in_range((new_pos := add_direction(guard_pos, direction)), max_x, max_y):
        if new_pos in obstacles:
            direction = _rotate(direction)
        else:
            guard_pos = new_pos
            visited.add(new_pos)

    return len(visited)

def add_direction(guard_pos, direction):
    return guard_pos[0] + direction[0], guard_pos[1]+direction[1]

def _is_in_range(guard_pos, max_x, max_y):
    return 0 <= guard_pos[0] <= max_x and 0 <= guard_pos[1] <= max_y

def _rotate(direction):
    match direction:
        case (0,1):
            return (1,0)
        case (1,0):
            return (0,-1)
        case (0,-1):
            return (-1,0)
        case _:
            return (0,1)

def puzzle2(input_file: Path):
    obstacles = set()
    guard_pos = None
    lines = input_file.read_text().splitlines()
    for y, line in zip(range(len(lines)-1, -1, -1), lines):
        for x, char in enumerate(line):
            if char == "#":
                obstacles.add(
                    (x,y) 
                )
            if char == "^":
                guard_pos = (x,y)

    max_y, max_x = len(lines)-1, len(lines[0])-1

    print_map(max_x, max_y, obstacles, set())
    assert guard_pos is not None
    print(max_y, max_x, guard_pos)
    direction = (0,1)
    visited:set[tuple[int,int]] = {guard_pos}
    loops = set()
    while _is_in_range((new_pos := add_direction(guard_pos, direction)), max_x, max_y):
        if new_pos in obstacles:
            direction = _rotate(direction)
        else:
            if _is_allowed(new_pos, visited) and is_loop_created(guard_pos, direction, max_x, max_y, {*obstacles, new_pos}):
                loops.add(new_pos)
            guard_pos = new_pos
            visited.add(new_pos)

    return len(loops)

def _is_allowed(new_pos, visited):
    if new_pos in visited:
        return False
    return True


def print_map(max_x, max_y, obstacles, visited):
    map_ = [
        ["." for _ in range(max_x+1)]
        for _ in range(max_y+1)
    ]
    for x, y in obstacles:
        map_[max_y-y][x] = "#"

    for ((x, y), _) in visited:
        map_[max_y-y][x] = "*"

    print("\n".join("".join(row) for row in map_), end="\n\n")

def is_loop_created(guard_pos, direction, max_x, max_y, obstacles):
    visited = {(guard_pos, direction)}
    while _is_in_range((new_pos := add_direction(guard_pos, direction)), max_x, max_y):
        if (new_pos, direction) in visited:
            return True
        if new_pos in obstacles:
            direction = _rotate(direction)
        else:
            guard_pos = new_pos
        visited.add((guard_pos, direction))

    return False

if __name__ == "__main__":
    print("Day 6")

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