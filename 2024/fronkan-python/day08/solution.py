from argparse import ArgumentParser
from pathlib import Path

def puzzle1(input_file: Path):
    grid = [[c for c in line] for line in input_file.read_text().splitlines()]
    max_row = len(grid)
    max_col = len(grid[0])
    antennas = [
        (ri,ci)
        for ri, row in enumerate(grid)
        for ci, elem in enumerate(row)
        if elem != "."
    ]

    antinodes = {
        potential_antinode
        for ri, ci in antennas
        for potential_antinode in find_antinodes(ri, ci, grid, max_row, max_col)
        if _is_in_range(potential_antinode, max_row, max_col)
    }

    print("\n".join(
        "".join("#" if (ri, ci) in antinodes and cell =="." else cell for ci, cell in enumerate(row))
        for ri, row in enumerate(grid)
    ))

    return len(antinodes)


def _is_in_range(potential_antinode, max_row, max_col):
    ri, ci = potential_antinode
    return all((
        0 <= ri < max_row,
        0 <= ci < max_col,
    ))

def find_antinodes(ri, ci, grid, max_row, max_col):
    antenna_type = grid[ri][ci]
    # for dc in range(1, ceil((max_col-ci)/2)):
    for dc in range(1, max_col-ci):
        if grid[ri][ci+dc] == antenna_type:
            yield (ri, ci+(dc*2))
            yield (ri, ci-(dc*2))

    # for dr in range(1, ceil((max_row-ri)/2)):
    for dr in range(1, max_row-ri):
        if grid[ri+dr][ci] == antenna_type:
            yield (ri+(dr*2), ci)
            yield (ri-(dr*2), ci)

    for dr in range(1, max_row-ri):
        for dc in range(-ci, max_col-ci):            
            if not dr and not dc:
                # We are on starting node
                continue

            if grid[ri+dr][ci+dc] == antenna_type:
                yield(ri+(dr*2), ci+(dc*2))
                yield(ri-(dr), ci-(dc))


def puzzle2(input_file: Path):
    grid = [[c for c in line] for line in input_file.read_text().splitlines()]
    max_row = len(grid)
    max_col = len(grid[0])
    antennas = [
        (ri,ci)
        for ri, row in enumerate(grid)
        for ci, elem in enumerate(row)
        if elem != "."
    ]

    antinodes = set()
    for ri, ci in antennas:
        for potential_antinode in find_antinodes2(ri, ci, grid, max_row, max_col):
            if _is_in_range(potential_antinode, max_row, max_col):
                antinodes.add(potential_antinode)
                antinodes.add((ri,ci))


    print("\n".join(
        "".join("#" if (ri, ci) in antinodes and cell =="." else cell for ci, cell in enumerate(row))
        for ri, row in enumerate(grid)
    ))

    return len(antinodes)

def find_antinodes2(ri, ci, grid, max_row, max_col):
    antenna_type = grid[ri][ci]
    for dc in range(1, max_col-ci):
        if grid[ri][ci+dc] == antenna_type:
            i = 0
            while True:
                p1 = (ri, ci+(dc*i))
                p2 = (ri, ci-(dc*i))
                if not _is_in_range(p1, max_row, max_col) and not _is_in_range(p2, max_row, max_col):
                    break
                yield p1
                yield p2
                i += 1

    for dr in range(1, max_row-ri):
        if grid[ri+dr][ci] == antenna_type:
            i = 0
            while True:
                p1 = (ri+(dr*i), ci)
                p2 = (ri-(dr*i), ci)
                if not _is_in_range(p1, max_row, max_col) and not _is_in_range(p2, max_row, max_col):
                    break
                yield p1
                yield p2
                i += 1

    for dr in range(0, max_row-ri):
        for dc in range(-ci, max_col-ci):            
            if not dr and not dc:
                # We are on starting node
                continue

            if grid[ri+dr][ci+dc] == antenna_type:
                i = 0
                while True:
                    p1 = (ri+(dr*i), ci+(dc*i))
                    p2 = (ri-(dr*i), ci-(dc*i))
                    if not _is_in_range(p1, max_row, max_col) and not _is_in_range(p2, max_row, max_col):
                        break
                    yield p1
                    yield p2
                    i += 1


if __name__ == "__main__":
    print("Day 8")

    parser = ArgumentParser()
    parser.add_argument("--example", action="store_true", default=False)
    args = parser.parse_args()

    if args.example:
        print("running example file")
        # input_file = Path(__file__).parent / "example_input.txt"
        input_file = Path(__file__).parent / "example_simple.txt"

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