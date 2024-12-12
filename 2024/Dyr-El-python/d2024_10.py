from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D, Vec2D
from collections import namedtuple, deque


def parse(inp: str):
    grid = Grid2D(inp)
    return grid


def part1(inp):
    grid = parse(inp)
    remaining = deque(grid.find(lambda pos, ch: ch == "0"))
    leads = {pos: {pos} for pos in remaining}
    correct_order = {c1: c2 for c1, c2 in zip("012345678", "123456789")}
    while remaining:
        pos = remaining.popleft()
        for step in Grid2D.four_directions():
            next_pos = pos + step
            if next_pos not in grid:
                continue
            if correct_order.get(grid[pos], "") != grid[next_pos]:
                continue
            leads[next_pos] = leads.get(next_pos, set()) | leads[pos]
            remaining.append(next_pos)
    ends = grid.find(lambda _, ch: ch == "9")
    return sum((len(leads[end]) for end in ends))


def part2(inp):
    grid = parse(inp)
    remaining = deque(grid.find(lambda pos, ch: ch == "0"))
    paths_to = {pos: {(pos,)} for pos in remaining}
    correct_order = {c1: c2 for c1, c2 in zip("012345678", "123456789")}
    while remaining:
        pos = remaining.popleft()
        for step in Grid2D.four_directions():
            next_pos = pos + step
            if next_pos not in grid:
                continue
            if correct_order.get(grid[pos], "") != grid[next_pos]:
                continue
            new_paths = {x + (next_pos,) for x in paths_to[pos]}
            paths_to[next_pos] = paths_to.get(next_pos, set()) | new_paths
            remaining.append(next_pos)
    ends = list(grid.find(lambda _, ch: ch == "9"))
    return sum((len(paths_to[end]) for end in ends))


ex_inp = """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732""".strip()


def test_1_1():
    expected = 36
    assert part1(ex_inp) == expected


def test_1_2():
    expected = 81
    assert part2(ex_inp) == expected


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 10)
    main(prep.get_content())
