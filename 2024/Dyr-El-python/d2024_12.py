from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D, Vec2D
from collections import namedtuple, deque
from functools import cache


def part1(inp):
    grid = Grid2D(inp)
    regions = list()
    covered = set()
    for pos, _ in grid.items():
        if pos not in covered:
            region = grid.find_region(pos)
            covered |= region
            regions.append(region)
    tot = 0
    for region in regions:
        length_fence = sum(
            (
                grid[pos] != grid.get(pos + d, ".")
                for pos in region
                for d in Grid2D.four_directions()
            )
        )
        tot += length_fence * len(region)
    return tot


def calc_side(grid, pos, d):
    ch = grid[pos]
    if grid.get(pos + d, ".") == ch:
        return None
    s = {pos}
    for d1 in (d >> 1, d << 1):
        p = pos + d1
        while grid.get(p, ".") == ch and grid.get(p + d, ".") != ch:
            s.add(p)
            p = p + d1
    return (d, frozenset(s))


def part2(inp):
    grid = Grid2D(inp)
    regions = list()
    found = set()
    for pos, ch in grid.items():
        if pos not in found:
            region = grid.find_region(pos)
            found |= region
            regions.append(region)
    tot = 0
    for region in regions:
        sides = set()
        for pos in region:
            for d in Grid2D.four_directions():
                if side := calc_side(grid, pos, d):
                    sides.add(side)
        tot += (len(sides)) * len(region)
    return tot


ex_inp = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE""".strip()


def test_1_1():
    expected = 1930
    assert part1(ex_inp) == expected


def test_1_2():
    expected = 1206
    assert part2(ex_inp) == expected


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 12)
    main(prep.get_content())
