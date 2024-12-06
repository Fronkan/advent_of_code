from aoc_prepare import PrepareAoc
from utils import Vec2D, Grid2D


def part1(inp):
    grid = Grid2D(inp)
    for pos, ch in grid.items():
        if ch in "v^<>":
            start_pos = pos
            break
    d = {"^": Vec2D(0, -1)}[grid[start_pos]]
    count = {(pos.x, pos.y)}
    while grid.get(pos, ' ') != ' ':
        if grid.get(pos + d, " ") != "#":
            pos = pos + d
            count.add((pos.x, pos.y))
        else:
            d = d << 1    
    return len(count) - 1


def part2(inp):
    import pudb; pu.db
    grid = Grid2D(inp)
    starting_pos = grid.find(lambda _, c: c in "v^<>")[0]
    starting_dir = {"^": Vec2D(0, -1), "v": Vec2D(0, -1), "<": Vec2D(-1, 0), ">": Vec2D(1, 0)}[grid[starting_pos]]
    print(f"{starting_pos=}, {starting_dir=}")
    path = []
    pos, direction = starting_pos, starting_dir
    while grid.get(pos, ' ') != ' ':
        if grid.get(pos + direction, " ") != "#":
            pos = pos + direction
            path.append(pos)
        else:
            direction = direction >> 1
    loop_found = 0
    for obstacle in set(path):
        if grid.get(obstacle, " ") == " ":
            continue
        pos = starting_pos
        d = starting_dir
        loop = set()
        while True:
            if grid.get(pos + d, " ") != "#" and pos + d != obstacle:
                pass
            else:
                d = d << 1
                continue
            pos = pos + d
            if grid.get(pos, ' ') == ' ':
                if obstacle == Vec2D(3, 6):
                    input()
                break
            if (pos.x, pos.y, d.x, d.y) in loop:
                loop_found += 1
                break
            loop.add((pos.x, pos.y, d.x, d.y))

    return loop_found


ex_1 = """
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
""".strip()

def test_1_1():
    assert part1(ex_1) == 41


def test_1_2():
    assert part2(ex_1) == 6


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 6)
    main(prep.get_content())
