from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D
from itertools import combinations, count
from functools import reduce


def part1(inp):
    grid = Grid2D(inp)
    antenna_types = {ch for _, ch in grid.items() if ch != "."}
    antennas = {
        antenna_type: grid.find(lambda _, x: x == antenna_type)
        for antenna_type in antenna_types
    }
    antinodes = set()
    for antenna_type in antennas:
        for pos1, pos2 in combinations(antennas[antenna_type], 2):
            for i in (-1, 2):
                antinode = pos1 + (pos2 - pos1) * i
                if antinode in grid:
                    antinodes.add(antinode)
    return len(antinodes)


def part2(inp):
    grid = Grid2D(inp)
    antenna_types = {ch for _, ch in grid.items() if ch != "."}
    antennas = {
        antenna_type: grid.find(lambda _, x: x == antenna_type)
        for antenna_type in antenna_types
    }
    antinodes = set()
    for antenna_type in antennas:
        for pos1, pos2 in combinations(antennas[antenna_type], 2):
            for i in count():
                antinode = pos1 + (pos2 - pos1) * i
                if antinode in grid:
                    antinodes.add(antinode)
                else:
                    break
            for i in count():
                antinode = pos1 + (pos2 - pos1) * (-i)
                if antinode in grid:
                    antinodes.add(antinode)
                else:
                    break
    return len(antinodes)


ex_inp = """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............""".strip()


def test_1_1():
    expected = 14
    assert part1(ex_inp) == expected


def test_1_2():
    expected = 34
    assert part2(ex_inp) == expected


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 8)
    main(prep.get_content())
